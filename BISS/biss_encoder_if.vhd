-------------------------------------------------------------------------
--
-- File name    :  
--              :
-- Comments     : 
--              :
-- Developers   : RAFAEL
--              :
-- Notes        :
-- ----------------------------------------------------------------------
-- Revision History :
-- ----------------------------------------------------------------------
--   Ver:  | Author:                 | Mod. Date :|    Changes Made:
--   v1.0  | O.Z.                   :| 01/06/2015:| 
-- ----------------------------------------------------------------------
--   Ver:  | Author:                 | Mod. Date :|    Changes Made:
--   v2.0  | C.S.                   :| 15/03/2016:| replacing generics to inputs (from registers)
--                                                | module will be a part of top module that have registers an av_mm if
-- ----------------------------------------------------------------------
--   Ver:  | Author:                 | Mod. Date :|    Changes Made:
--   v3.0  | O.Z.                   :| 30/12/2020:| ENA input added.
--                                                | END_TIMEOUT_DELAY input added
-- ----------------------------------------------------------------------
--   Ver:  | Author:                 | Mod. Date :|    Changes Made:
--   v4.0  | O.B.                   :| 31/07/2022:| DUPLEX CUMMNICATION ADDED.
--                                                | BISS SAFTEY protcol support added.
-- ----------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
--use ieee.numeric_std.all;
library work;
use work.BISS_ENC_PKG.all;
--

entity biss_encoder_if is
	generic(
		G_RESET_ACTIVE_VALUE : std_logic := '1'; -- polarity of reset
		G_BISS_MAX_DATA_BITS : integer   := 40
	);
	port(
		CLK                    : in  std_logic; -- system clock
		RST                    : in  std_logic; -- asynchronous reset
		ENA                    : in  std_logic; -- active high
		START                  : in  std_logic; -- active high start signal - initiate the read process
		BISS_TYPE              : in  std_logic; -- biss mode
		SNIFFER_MODE 		   : in  std_logic; -- master mode/sniffer moder
		-- enc signals
		MA                     : out std_logic; -- encoder clock
		MA_IN				   : in  std_logic; -- encoder clock for sniffer operation_
		SLO                    : in  std_logic; -- encoder serial data
		-- config inputs --			
		RESOLUTION             : in  std_logic_vector(5 downto 0); -- encoder resolution
		--CRC_WIDTH              : in  std_logic_vector(4 downto 0); -- encoder crc resolution
		MA_TO_CLK_RATIO        : in  std_logic_vector(7 downto 0); -- number of input clocks per MA cycle 
		ACK_TIMEOUT_DELAY      : in  std_logic_vector(7 downto 0); -- number of MA cycle to yield 21us dely 
		END_TIMEOUT_DELAY      : in  std_logic_vector(8 downto 0); -- number of MA cycle to yield 40us timeout at end of cycle
		RESET_EN               : in  std_logic_vector(4 downto 0); -- IN which type of error ,It will enter to reset state (active low)
		RESET_TIME_STATE       : in  std_logic_vector(15 downto 0); -- reset time at us (MA on high)
		CDM_ADDRESS            : in  std_logic_vector(6 downto 0); -- Master address sent	
		CDM_OPERATION          : in  std_logic_vector(1 downto 0); -- Read /Write operation 
		CDM_DATA               : in  std_logic_vector(7 downto 0); -- Master data sent out
		-- output to registers/out of the module
		ENCODER_POSITION       : out std_logic_vector(G_BISS_MAX_DATA_BITS - 1 downto 0); -- encoder position
		ENCODER_POSITION_VALID : out std_logic; -- active high encoder position valid
		ENCODER_LIFE_POSITION  : out std_logic_vector(5 downto 0);
		--  ENCODER_ERROR               : out   std_logic;                                          -- encoder error
		ENCODER_STATUS         : out std_logic_vector(7 downto 0);
		--  CRC_ERROR                   : out   std_logic                                           -- crc error
		CDS_DATA               : out std_logic_vector(7 downto 0)
	);
end entity;

architecture synt of biss_encoder_if is

	type encoder_if_states is (
		st_encoder_reset,
		st_idle,
		st_req_position_acquisition,
		st_check_ack,
		st_wait_for_start,
		st_control_data_slave,
		st_read_encoder_data,
		st_wait_for_timeout_end
	);

	--register sampling signals. sampled once per read cycle on st_idle
	signal ma_to_clk_ratio_val_from_reg   : integer range 0 to 2**(MA_TO_CLK_RATIO'high + 1) - 1;
	signal half_ma_to_clk_ratio_from_reg  : integer range 0 to 2**(MA_TO_CLK_RATIO'high) - 1;
	signal ack_timeout_delay_val_from_reg : integer range 0 to 2**(ACK_TIMEOUT_DELAY'high + 1) - 1;
	signal total_data_bits                : integer range 0 to C_MAX_TOTAL_DATA_BITS - 1;
	signal master_message_bits            : integer range 0 to C_MAX_TOTAL_MASTER_MESSAGE_BITS - 1;
	signal reset_time_period	      : integer range 0 to 2**(RESET_TIME_STATE'high+1)-1;
	signal startup			      : std_logic;

	signal ma_to_clk_ratio_counter : integer range 0 to 2**(MA_TO_CLK_RATIO'high + 1) - 1;
	signal bits_counter            : integer range 0 to 2**(END_TIMEOUT_DELAY'high + 1) - 1;

	signal encoder_if_sm             : encoder_if_states;
	signal slo_int                   : std_logic;
	signal ma_int                    : std_logic;
	signal encoder_data_reg          : std_logic_vector(C_MAX_TOTAL_DATA_BITS - 1 downto 0);
	signal encoder_data_reg_valid    : std_logic;
	signal crc_calc_in_val           : std_logic;
	signal calculated_crc_reg        : std_logic_vector(15 downto 0);
	signal start_sample_cnt          : integer range 0 to 2**(RESET_TIME_STATE'high+1)-1;
	signal encoder_error_int         : std_logic;
	signal crc_error_int             : std_logic;
	signal encoder_ack_error         : std_logic := '0';
	signal encoder_start_error       : std_logic := '0';
	signal encoder_timeout_end_error : std_logic := '0';
	signal message_data_reg          : std_logic_vector(C_MAX_TOTAL_MASTER_MESSAGE_BITS - 1 downto 0);

	alias encoder_position_value          : std_logic_vector(G_BISS_MAX_DATA_BITS-1 downto 0) is encoder_data_reg(encoder_data_reg'left downto encoder_data_reg'left - G_BISS_MAX_DATA_BITS + 1);
	signal encoder_warning_value          : std_logic;
	alias encoder_crc_value               : std_logic_vector(C_NUM_OF_CRC_BITS-1 downto 0) is encoder_data_reg(C_NUM_OF_CRC_BITS - 1 downto 0);
	signal encoder_error_value            : std_logic;
	signal message_operation              : std_logic_vector(1 downto 0) := "00";
	signal calculated_crc_cdm_data_reg    : std_logic_vector(3 downto 0);
	signal calculated_crc_cdm_address_reg : std_logic_vector(3 downto 0);
	signal message_valid                  : std_logic;
	signal message_finished               : std_logic;
	signal encoder_warning_int            : std_logic;
	signal cds_data_reg                   : std_logic_vector(7 downto 0);
	signal crc_counter                    : integer range 0 to 8;
	signal message_done                   : std_logic;
	signal update_bit                     : std_logic;
	signal sig_ma_fall                    : std_logic;
	signal sig_ma_fff                     : std_logic;
	signal sig_ma_ff                      : std_logic;
	signal sig_ma_f                       : std_logic;
	signal sig_ma_rise                    : std_logic;
	signal sig_ma_source_rise             : std_logic;
	signal ma_rise                        : std_logic;
	signal ma_fall : std_logic;
	signal sig_ma_fall_rise : std_logic;
	signal sig_ma_source_fall : std_logic;
	
begin


	-----------------------------------------------------------------------------------------------------------
	-- gen_encoder_if_sm encoder state machine
	-----------------------------------------------------------------------------------------------------------
	gen_encoder_if_sm : process(CLK, RST)
	begin
		if RST = G_RESET_ACTIVE_VALUE then
			--ma_to_clk_ratio_counter <= 0;
			bits_counter            <= 0;
			encoder_if_sm           <= st_encoder_reset;
			start_sample_cnt        <= 0;
			master_message_bits     <= 0;
			update_bit 		<= '0';

			ma_to_clk_ratio_val_from_reg   <= 0;
			half_ma_to_clk_ratio_from_reg  <= 0;
			total_data_bits                <= 0;
			ack_timeout_delay_val_from_reg <= 0;
			encoder_ack_error              <= '0';
			encoder_start_error            <= '0';
			encoder_timeout_end_error      <= '0';
			startup<='1';
		elsif rising_edge(CLK) then

			if ENA = '0' then
				--ma_to_clk_ratio_counter <= 0;
				bits_counter            <= 0;
				encoder_if_sm           <= st_encoder_reset;
				start_sample_cnt        <= 0;
				update_bit 		<= '0';
				ma_to_clk_ratio_val_from_reg   <= 0;
				half_ma_to_clk_ratio_from_reg  <= 0;
				total_data_bits                <= 0;
				master_message_bits            <= 0;
				ack_timeout_delay_val_from_reg <= 0;
				reset_time_period              <= 0;
				startup<='1';
				
			else

				case encoder_if_sm is

					when st_encoder_reset =>
					    if (startup = '1') then -- first wake up

                            if start_sample_cnt = C_SAMPLE_PERIOD - 1 then
                                start_sample_cnt <= 0;
                                encoder_if_sm    <= st_idle;
                                reset_time_period <= conv_integer(RESET_TIME_STATE);
                                startup <='0';
                            else
                                start_sample_cnt <= start_sample_cnt + 1;
                            end if;
                        elsif (startup ='0') then
                            
                            if start_sample_cnt = reset_time_period then
                                start_sample_cnt <= 0;
                                encoder_if_sm    <= st_idle;
                            else
                                start_sample_cnt <= start_sample_cnt + 1;
                            end if;

                        end if;
					
						--                      ma_clk_counter_ena <= '0';
						master_message_bits <= C_MAX_TOTAL_MASTER_MESSAGE_BITS - 1;
						update_bit 		<= '0';

					when st_idle =>
						--sample cfg regs
						ma_to_clk_ratio_val_from_reg  <= conv_integer(MA_TO_CLK_RATIO);
						half_ma_to_clk_ratio_from_reg <= conv_integer(MA_TO_CLK_RATIO(MA_TO_CLK_RATIO'high downto 1));

						if (BISS_TYPE = '0') then
							total_data_bits <= conv_integer(RESOLUTION) + C_FIXED_DATA_BITS + conv_integer(C_CRC_WIDTH_NORMAL) - 1;
						else
							total_data_bits <= conv_integer(RESOLUTION) + C_FIXED_DATA_BITS + conv_integer(C_CRC_WIDTH_SAFETY) - 1 + C_NUM_OF_LIFE_COUNTER;
						end if;
						ack_timeout_delay_val_from_reg <= conv_integer(ACK_TIMEOUT_DELAY);

						-- wait for start signal
						if (START = '1' and SNIFFER_MODE = '0') or (sig_ma_source_fall = '1' and SNIFFER_MODE='1') then
							encoder_if_sm             <= st_req_position_acquisition;
							--ma_to_clk_ratio_counter   <= conv_integer(MA_TO_CLK_RATIO) - 1;
							if (SNIFFER_MODE ='0') then
							bits_counter              <= 1;
						    else
						    bits_counter              <= 0;
						    end if;
							encoder_ack_error         <= '0';
							encoder_start_error       <= '0';
							encoder_timeout_end_error <= '0';
						end if;

					when st_req_position_acquisition =>

						if ((sig_ma_source_rise ='1' and SNIFFER_MODE ='0') or (sig_ma_source_fall ='1' and SNIFFER_MODE ='1') ) then
							if bits_counter = 0 then
								encoder_if_sm <= st_check_ack;

							else
								bits_counter <= bits_counter - 1;
							end if;
						end if;

					when st_check_ack =>

						if ((sig_ma_source_rise ='1' and SNIFFER_MODE ='0') or (sig_ma_source_fall ='1' and SNIFFER_MODE ='1') ) then -- consider checking that SLO is '0' else reset
							if slo_int = '0' then
								encoder_if_sm <= st_wait_for_start;
							else
							    if (RESET_EN(2) ='0') then --ack error
								encoder_if_sm     <= st_encoder_reset;
							    else
							    encoder_if_sm <= st_wait_for_start;
							    end if;
								encoder_ack_error <= '1';
							end if;
							bits_counter            <= ack_timeout_delay_val_from_reg;
						end if;

					when st_wait_for_start =>

						if ((sig_ma_source_rise ='1' and SNIFFER_MODE ='0') or (sig_ma_source_fall ='1' and SNIFFER_MODE ='1') )then
							if slo_int = '1' then
								encoder_if_sm <= st_control_data_slave;
							elsif bits_counter = 0 then
								encoder_if_sm       <= st_wait_for_timeout_end;
								encoder_start_error <= '1';
								bits_counter        <= conv_integer(END_TIMEOUT_DELAY);
							else
								bits_counter <= bits_counter - 1;
							end if;
						end if;

					when st_control_data_slave =>
						if ((sig_ma_source_rise ='1' and SNIFFER_MODE ='0') or (sig_ma_source_fall ='1' and SNIFFER_MODE ='1') ) then
								encoder_if_sm <= st_read_encoder_data;
								bits_counter  <= total_data_bits;
						end if;

					when st_read_encoder_data =>

						if ((sig_ma_source_rise ='1' and SNIFFER_MODE ='0') or (sig_ma_source_fall ='1' and SNIFFER_MODE ='1'))  then
							if bits_counter = 0 then
								encoder_if_sm <= st_wait_for_timeout_end;
								update_bit <= '1';
								
								bits_counter  <= conv_integer(END_TIMEOUT_DELAY);
							else
								bits_counter <= bits_counter - 1;
							end if;
						end if;

					when st_wait_for_timeout_end =>
						
					if ma_to_clk_ratio_counter = 0 then
                            if update_bit = '1' then -- can cancel update_bit 

                                update_bit <= '0';
                                if message_valid = '1' then

                                    if (master_message_bits = 0) then
                                        master_message_bits <= C_MAX_TOTAL_MASTER_MESSAGE_BITS - 1;
                                    else
                                        master_message_bits <= master_message_bits - 1;
                                    end if;

                                else
                                    master_message_bits <= C_MAX_TOTAL_MASTER_MESSAGE_BITS - 1;
                                end if;

                            end if;

						if slo_int = '1' then
							if ((encoder_error_int = '1' and RESET_EN(0) ='0') or (crc_error_int = '1' and RESET_EN(1) ='0')) then
								encoder_if_sm <= st_encoder_reset;
							else
								encoder_if_sm <= st_idle;
							end if;
						elsif bits_counter = 0 then
							
						if (RESET_EN(4) ='0') then
                            encoder_if_sm             <= st_encoder_reset;
                        else
                            encoder_if_sm <= st_idle;
                        end if;
                        
							encoder_timeout_end_error <= '1';
						else
							bits_counter <= bits_counter - 1;
						end if;

                end if;
				end case;
			end if;

		end if;
	end process;

	-----------------------------------------------------------------------------------------------------------
	-- i\f process
	-----------------------------------------------------------------------------------------------------------
	process(CLK, RST)
	begin
		if RST = G_RESET_ACTIVE_VALUE then
			ma_int                 <= '1';
			slo_int                <= '1';
			encoder_data_reg       <= (others => '0');
			encoder_data_reg_valid <= '0';
			ENCODER_POSITION       <= (others => '0');
			ENCODER_POSITION_VALID <= '0';
			ENCODER_LIFE_POSITION  <=(others=>'0');
			crc_error_int          <= '0';
			encoder_error_int      <= '0';
			encoder_warning_int    <= '0';
			cds_data_reg 		   <= (others =>'0');
			
		elsif rising_edge(CLK) then
			
			slo_int                <= SLO;
			encoder_data_reg_valid <= '0';
			ENCODER_POSITION_VALID <= '0';
						---------------------------------------------------------------------------------------------------------------------------
			---------------------------------------CLOCK OUT MA SIGNAL LOGIC-----------------------------------------------------------
			---------------------------------------------------------------------------------------------------------------------------
			if (encoder_if_sm /= st_wait_for_timeout_end) and (encoder_if_sm /= st_encoder_reset) and (encoder_if_sm /= st_idle) then
				if ma_to_clk_ratio_counter = 0 then
					ma_int <= '1';
					ma_fall<= '0';
					ma_rise<= '1';
				elsif ma_to_clk_ratio_counter = half_ma_to_clk_ratio_from_reg then
					ma_int <= '0';
					ma_fall<= '1';
					ma_rise<= '0';
				else
					ma_fall<= '0';
					ma_rise<= '0';
				end if;
			elsif (encoder_if_sm = st_wait_for_timeout_end) then
				if ma_to_clk_ratio_counter = half_ma_to_clk_ratio_from_reg then
					ma_int <= not  message_data_reg(master_message_bits);
				end if;
				ma_fall<= '0';
			    ma_rise<= '0';	
			else	
				ma_int <= '1';
				ma_fall<= '0';
			    ma_rise<= '0';				
			end if;
			---------------------------------------------------------------------------------------------------------------------------
			---------------------------------------SLO LOGIC---------------------------------------------------------------------------
			---------------------------------------------------------------------------------------------------------------------------
			if encoder_if_sm = st_idle then
				encoder_data_reg <= (others => '0');
			elsif encoder_if_sm = st_read_encoder_data and sig_ma_source_rise ='1' then --slo_valid = '1' then
				encoder_data_reg(bits_counter) <= slo_int;
				if bits_counter = 0 then
					encoder_data_reg_valid <= '1';
				end if;
			elsif encoder_if_sm = st_control_data_slave and sig_ma_source_rise ='1' then
				if ((master_message_bits <= C_MAX_TOTAL_MASTER_MESSAGE_BITS - C_CDS_POSITION-1) and (master_message_bits >= C_MAX_TOTAL_MASTER_MESSAGE_BITS - C_CDS_POSITION - CDS_DATA'length)) then
					cds_data_reg(master_message_bits - 5) <= slo_int;
				end if;
			end if;
			
			
			
			if encoder_data_reg_valid = '1' then
				
				if (BISS_TYPE ='0') then 
				ENCODER_POSITION <=  encoder_data_reg(G_BISS_MAX_DATA_BITS+conv_integer(C_CRC_WIDTH_NORMAL)+C_FIXED_DATA_BITS -1 downto conv_integer(C_CRC_WIDTH_NORMAL)+C_FIXED_DATA_BITS );
				ENCODER_LIFE_POSITION <= C_ZEROS(5 downto 0); 
				elsif (BISS_TYPE = '1') then
				ENCODER_POSITION <=  encoder_data_reg(G_BISS_MAX_DATA_BITS+conv_integer(C_CRC_WIDTH_SAFETY)+C_NUM_OF_LIFE_COUNTER+C_FIXED_DATA_BITS -1 downto conv_integer(C_CRC_WIDTH_SAFETY)+C_NUM_OF_LIFE_COUNTER+C_FIXED_DATA_BITS );
				ENCODER_LIFE_POSITION <= encoder_data_reg(conv_integer(C_CRC_WIDTH_SAFETY) + C_NUM_OF_LIFE_COUNTER  - 1 downto conv_integer(C_CRC_WIDTH_SAFETY) );
				end if ;
					
				ENCODER_POSITION_VALID <= '1';

				if (calculated_crc_reg = 2 and BISS_TYPE = '0') or (calculated_crc_reg = x"E82E" and BISS_TYPE = '1') then -- check crc
					crc_error_int <= '0';
				else
					crc_error_int <= '1';
				end if;

				encoder_error_int <= not encoder_error_value; -- encoder ERROR and WARNING bits are active low
				encoder_warning_int <= not encoder_warning_value; -- encoder ERROR and WARNING bits are active low
			end if;

		end if;
	end process;

-------------------------------------------------------------------------------------------------------------------------------------------
--- CRC CALCULATION OF PACKET CRC_6(Poly X^6+X=1) OR CRC_16(Poly X^16+X^15+X^12+X^7+X^6+X^4+X^3+1)-----------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------

	crc_calc_in_val <= slo_int xor calculated_crc_reg(5) When BISS_TYPE = '0' else calculated_crc_reg(15) xor slo_int;

	calculate_crc : process(CLK, RST)
	begin
		if RST = G_RESET_ACTIVE_VALUE then
			calculated_crc_reg <= (others => '0');
		elsif rising_edge(CLK) then
			if encoder_if_sm = st_wait_for_start then
				calculated_crc_reg <= (others => '0');
			elsif encoder_if_sm = st_read_encoder_data and sig_ma_source_rise ='1' then --slo_valid = '1' then
				if (BISS_TYPE = '0') then 
				calculated_crc_reg(5 downto 2) <= calculated_crc_reg(4 downto 1);
				calculated_crc_reg(1)          <= calculated_crc_reg(0) xor crc_calc_in_val;
				calculated_crc_reg(0)          <= crc_calc_in_val;
				end if;
				if (BISS_TYPE = '1') then
					calculated_crc_reg(15)         <= calculated_crc_reg(14) xor crc_calc_in_val;
					calculated_crc_reg(14 downto 13)<= calculated_crc_reg(13 downto 12);
					calculated_crc_reg(12)         <= calculated_crc_reg(11) xor crc_calc_in_val;
					calculated_crc_reg(11 downto 8)<= calculated_crc_reg(10 downto 7);
					calculated_crc_reg(7)          <= calculated_crc_reg(6) xor crc_calc_in_val;
					calculated_crc_reg(6)          <= calculated_crc_reg(5) xor crc_calc_in_val;
					calculated_crc_reg(5)          <= calculated_crc_reg(4);
					calculated_crc_reg(4)          <= calculated_crc_reg(3) xor crc_calc_in_val;
					calculated_crc_reg(3)          <= calculated_crc_reg(2) xor crc_calc_in_val;
					calculated_crc_reg(2 downto 1) <= calculated_crc_reg(1 downto 0);
					calculated_crc_reg(0)          <= crc_calc_in_val;
				end if;
			end if;
		end if;
	end process;

-------------------------------------------------------------------------------------------------------------------------------------------
--- START THE CRC CDM CALCULATION ---------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------
	crc_counter_proc : process (CLK, RST) is
	begin
		if RST =  G_RESET_ACTIVE_VALUE then
			crc_counter <= 0;
		elsif rising_edge(CLK) then
			if (message_valid = '1') and (master_message_bits = C_MAX_TOTAL_MASTER_MESSAGE_BITS -2 ) then 
				if (crc_counter /= 8 ) then
				crc_counter<=crc_counter +1;
				end if;
			else
				crc_counter<=0;
			end if;	
		end if;
	end process crc_counter_proc;
	
-------------------------------------------------------------------------------------------------------------------------------------------
--- CRC CALCULATION OF CDM ADDRESS AND CDM DATA CRC_4(Poly X^4+X+1) -----------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------
	process(CLK, RST)
	begin
		if (RST = G_RESET_ACTIVE_VALUE) then
			calculated_crc_cdm_address_reg <= (others => '0');
		elsif rising_edge(CLK) then

			if (message_valid = '1') and (master_message_bits = C_MAX_TOTAL_MASTER_MESSAGE_BITS -2 ) and crc_counter <= 6 then
				calculated_crc_cdm_address_reg(0)<=CDM_ADDRESS(CDM_ADDRESS'high-crc_counter) xor calculated_crc_cdm_address_reg(3);
				calculated_crc_cdm_address_reg(1)<=calculated_crc_cdm_address_reg(0) xor CDM_ADDRESS(CDM_ADDRESS'high-crc_counter) xor calculated_crc_cdm_address_reg(3);
				calculated_crc_cdm_address_reg(2)<=calculated_crc_cdm_address_reg(1);
				calculated_crc_cdm_address_reg(3)<=calculated_crc_cdm_address_reg(2); 
			elsif master_message_bits = C_MAX_TOTAL_MASTER_MESSAGE_BITS -1 then
				calculated_crc_cdm_address_reg <= (others =>'0');
			end if; 
				
		end if;
	end process;
	
	process(CLK, RST)
	begin
		if (RST = G_RESET_ACTIVE_VALUE) then
			calculated_crc_cdm_data_reg <= (others => '0');
		elsif rising_edge(CLK) then

			if (message_valid = '1') and (master_message_bits = C_MAX_TOTAL_MASTER_MESSAGE_BITS -2 ) and crc_counter <= 7  then
				calculated_crc_cdm_data_reg(0)<=CDM_DATA(CDM_DATA'high-crc_counter) xor calculated_crc_cdm_data_reg(3);
				calculated_crc_cdm_data_reg(1)<=calculated_crc_cdm_data_reg(0) xor CDM_DATA(CDM_DATA'high-crc_counter) xor calculated_crc_cdm_data_reg(3);
				calculated_crc_cdm_data_reg(2)<=calculated_crc_cdm_data_reg(1);
				calculated_crc_cdm_data_reg(3)<=calculated_crc_cdm_data_reg(2); 
			elsif master_message_bits = C_MAX_TOTAL_MASTER_MESSAGE_BITS -1 then
				calculated_crc_cdm_data_reg <= (others =>'0');
			end if; 
		end if;
	end process;
-------------------------------------------------------------------------------------------------------------------------------------------
--- UPDATE THE MESSAGE INFO (CDM) PROCESS -------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------
--it 47 bits register that conatins the WRITE or READ commmand to BISS ENCODER  , no support burst write or read
--its need CDM_OPERATION need to go from "01"/"10" to "00" to start message operation

	message_update_process : process(CLK, RST) is
	begin
		if RST = G_RESET_ACTIVE_VALUE then
			message_valid     <= '0';
			message_done      <= '0';
			message_operation <= "00";
			message_data_reg  <= (others => '0');
			CDS_DATA          <= (others => '0');
		elsif rising_edge(CLK) then
			if (message_operation = "01") and CDM_OPERATION = "00" and master_message_bits = C_MAX_TOTAL_MASTER_MESSAGE_BITS - 1  and encoder_if_sm /=  st_encoder_reset then
				message_data_reg <= C_ZEROS(13 downto 0) & "11000" & CDM_ADDRESS & calculated_crc_cdm_address_reg & message_operation & "1" & CDM_DATA & calculated_crc_cdm_data_reg & "00"; --need to put crc
				message_valid    <= '1';
			elsif (message_operation = "10") and CDM_OPERATION = "00" and master_message_bits = C_MAX_TOTAL_MASTER_MESSAGE_BITS - 1  and encoder_if_sm /= st_encoder_reset then
				message_data_reg <= C_ZEROS(13 downto 0) & "11000" & CDM_ADDRESS & calculated_crc_cdm_address_reg & message_operation & "1" & C_ZEROS(11 downto 0) & "00"; --need to put crc
				message_valid    <= '1';
			elsif (master_message_bits = C_MAX_TOTAL_MASTER_MESSAGE_BITS - CDM_DATA'length - 2) then --update crc_calucaltion
				message_data_reg(20 downto 17) <=  calculated_crc_cdm_address_reg; --need to put crc
				if (message_data_reg(16 downto 15) = "01") then
					message_data_reg(5 downto 0) <=  calculated_crc_cdm_data_reg & "00";
				elsif (message_data_reg(16 downto 15) = "10") then
					message_data_reg(14 downto 0) <= "1" & C_ZEROS(13 downto 0);
				end if;
			elsif ((master_message_bits = C_MAX_TOTAL_MASTER_MESSAGE_BITS - 1 and encoder_if_sm = st_control_data_slave and sig_ma_source_fall  ='1' and message_done ='1')) then
				CDS_DATA <= cds_data_reg;
				message_done <='0';
			elsif (master_message_bits = 0 and encoder_if_sm = st_idle) then
				message_done <='1';
				message_valid <= '0';
			elsif (encoder_if_sm =  st_encoder_reset ) then 
				message_done <='0';
				message_valid <= '0';
			end if;
			message_operation   <= CDM_OPERATION;
		end if;
	end process message_update_process;
	
	
	
	--
	
	MA                         <= ma_int;
	--  ENCODER_ERROR <= encoder_error_int;
	--  CRC_ERROR     <= crc_error_int;
	ENCODER_STATUS(0)          <= encoder_error_int;
	ENCODER_STATUS(1)          <= crc_error_int;
	ENCODER_STATUS(2)          <= encoder_ack_error;
	ENCODER_STATUS(3)          <= encoder_start_error;
	ENCODER_STATUS(4)          <= encoder_timeout_end_error;
	ENCODER_STATUS(5)		   <= encoder_warning_int;
	ENCODER_STATUS(7 downto 6) <= (others => '0');

	--
	
	ma_to_clk_counter : process(CLK, RST) is
	begin
		if RST = G_RESET_ACTIVE_VALUE then
			ma_to_clk_ratio_counter <= 0;
		elsif rising_edge(CLK) then
			if ENA = '0' then
				ma_to_clk_ratio_counter <= 0;
			else
				if (START = '1' and encoder_if_sm = st_idle) then
					ma_to_clk_ratio_counter <= conv_integer(MA_TO_CLK_RATIO) - 1;
				elsif (encoder_if_sm /= st_idle) and (encoder_if_sm /= st_encoder_reset) then
					if ma_to_clk_ratio_counter = 0 then
						ma_to_clk_ratio_counter <= ma_to_clk_ratio_val_from_reg - 1;
					else
						ma_to_clk_ratio_counter <= ma_to_clk_ratio_counter - 1;
					end if;

				end if;
			end if;
		end if;
	end process ma_to_clk_counter;
	
	--
	
	sig_ma_fall <= sig_ma_fff and (not sig_ma_ff);
    sig_ma_rise <= sig_ma_ff and (not sig_ma_fff);
	-- 
    process(RST, CLK)
        begin
        if RST = G_RESET_ACTIVE_VALUE then
            sig_ma_f <= '0';
            sig_ma_ff <= '0';
            sig_ma_fff <= '0';
        elsif rising_edge(CLK) then
            sig_ma_f <= MA_IN;
            sig_ma_ff <= sig_ma_f;
            sig_ma_fff <= sig_ma_ff;
        end if;
    end process;
	
	--
	
	sig_ma_source_rise <= ma_rise when SNIFFER_MODE ='0' else sig_ma_rise;
	sig_ma_source_fall <= ma_fall when SNIFFER_MODE ='0' else sig_ma_fall;
	--
	
	encoder_error_value <= encoder_data_reg(conv_integer(C_CRC_WIDTH_NORMAL) + C_NUM_OF_WARNING_BITS + C_NUM_OF_ERROR_BITS - 1) when BISS_TYPE = '0' else encoder_data_reg(conv_integer(C_CRC_WIDTH_SAFETY) + C_NUM_OF_LIFE_COUNTER + C_NUM_OF_WARNING_BITS + C_NUM_OF_ERROR_BITS - 1);
	encoder_warning_value <= (encoder_data_reg(conv_integer(C_CRC_WIDTH_NORMAL) + C_NUM_OF_WARNING_BITS  - 1)) when BISS_TYPE = '0' else encoder_data_reg(conv_integer(C_CRC_WIDTH_SAFETY) + C_NUM_OF_LIFE_COUNTER + C_NUM_OF_WARNING_BITS  - 1);

end architecture;

