--------------------------------------------------------------------------------------------------------------------------------------------------
-- biss_encoder_top: the module controls the biss_encoder conunication -  
-- it contains:  biss_encoder_if - that receives configurations from the register mode and outputs the encoder position.
--               biss_enc_regs - register module, comunicat via AV_MM signals.
-- configuration regs are written by sw to biss_enc_regs and are sampled by biss_encoder_if once per position read cycle on st_idle.
-- biss_encoder_if reads the position from the encoder and pass it along with err indication to biss_enc_regs and out of the top module for othe fw use (simulinc)
-- if the encoder report error or a crc error is detected - the biss_encoder_if resets the encoder and restart the position read.
-- position is read periodiclly, a read cycle takes at least 40us.
--------------------------------------------------------------------------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.BISS_ENC_PKG.all;

entity biss_encoder_top is
generic (
    G_RESET_ACTIVE_VALUE        : std_logic := '1';                                         -- polarity of reset input
    G_START_POLARITY            : std_logic := '1';                                          -- polarity of start input
    G_CLK_FREQ_IN_MHZ           : integer := 0;
    G_BISS_MAX_DATA_BITS        : integer := 40

); 
port    (
    RESET                       : in    std_logic;                                          -- global synchronous reset, polarity defined by G_RESET_ACTIVE_VALUE
    CLK                         : in    std_logic;                                          -- clock input
    START                       : in    std_logic;                                          -- active high start signal - initiate the read process
    MODULE_EN                   : in    std_logic;                                          -- input enable,should be connected to const on top level, cant be changed on the fly.
    SNIFFER_MODE           : in  std_logic; -- master mode/sniffer mode 
    -- enc signals
    MA                          : out   std_logic;                                          -- encoder clock
    MA_IN                  : in  std_logic; -- encoder clock for sniffer operation_
    SLO                         : in    std_logic;                                          -- encoder serial data
    -- output to other moduls
    BISS_TYPE 					: in 	std_logic; 											-- biss mode
    ENCODER_POSITION            : out   std_logic_vector(G_BISS_MAX_DATA_BITS-1 downto 0);  -- encoder position
    ENCODER_POSITION_VALID      : out   std_logic;                                          -- active high encoder position valid
--  ENCODER_ERROR               : out   std_logic;
--  CRC_ERROR                   : out   std_logic;
    ENCODER_STATUS              : out   std_logic_vector(7 downto 0);
    ENCODER_ENABLED             : out   std_logic;
    --  av_mm slave signals
    AV_MM_WRITEDATA             : in    std_logic_vector(31 downto 0);                      -- writedata
    AV_MM_WRITE                 : in    std_logic;                                          -- write
    AV_MM_BYTEENABLE            : in    std_logic_vector(3 downto 0);                       -- byteenable
    AV_MM_WAITREQUEST           : out   std_logic;                                          -- waitrequest
    AV_MM_READDATA              : out   std_logic_vector(31 downto 0);                      -- readdata
    AV_MM_READDATAVALID         : out   std_logic;                                          -- readdatavalid
    AV_MM_ADDRESS               : in    std_logic_vector(C_BISS_ENC_ADDR_WIDTH-1 downto 0); -- address
    AV_MM_READ                  : in    std_logic                                           -- read
);
end entity;

architecture biss_encoder_top_arc of biss_encoder_top is


    component biss_enc_regs
    	generic(
        G_RESET_ACTIVE_VALUE        : std_logic := '1';                                         -- polarity of reset input
        G_CLK_FREQ_IN_MHZ           : integer := 0;
        G_BISS_MAX_DATA_BITS        : integer := 40
    	);
    	port(
    		RESET                  : in  std_logic;
    		CLK                    : in  std_logic;
    		ENCODER_LIFE_POSITION  : in 	std_logic_vector(5 downto 0);
    		ENCODER_POSITION       : in  std_logic_vector(G_BISS_MAX_DATA_BITS - 1 downto 0);
    		ENCODER_POSITION_VALID : in  std_logic;
    		ENCODER_STATUS         : in  std_logic_vector(7 downto 0);
    		CDS_DATA               : in  std_logic_vector(7 downto 0);
    		RESOLUTION             : out std_logic_vector(5 downto 0);
    		MA_TO_CLK_RATIO        : out std_logic_vector(7 downto 0);
    		ACK_TIMEOUT_DELAY      : out std_logic_vector(7 downto 0);
    		END_TIMEOUT_DELAY      : out std_logic_vector(8 downto 0);
    		ENCODER_ENA            : out std_logic;
    		CDM_ADDRESS            : out std_logic_vector(6 downto 0);
    		CDM_OPERATION          : out std_logic_vector(1 downto 0);
    		CDM_DATA               : out std_logic_vector(7 downto 0);
    		RESET_EN               : out  std_logic_vector(4 downto 0); -- IN which type of error ,It will enter to reset state (active low)
            RESET_TIME_STATE       : out  std_logic_vector(15 downto 0); -- reset time at us (MA on high)
    		AV_MM_WRITEDATA        : in  std_logic_vector(31 downto 0);
    		AV_MM_WRITE            : in  std_logic;
    		AV_MM_BYTEENABLE       : in  std_logic_vector(3 downto 0);
    		AV_MM_WAITREQUEST      : out std_logic;
    		AV_MM_READDATA         : out std_logic_vector(31 downto 0);
    		AV_MM_READDATAVALID    : out std_logic;
    		AV_MM_ADDRESS          : in  std_logic_vector(C_BISS_ENC_ADDR_WIDTH - 1 downto 0);
    		AV_MM_READ             : in  std_logic
    	);
    end component biss_enc_regs;
    

    component biss_encoder_if is
    generic (
        G_RESET_ACTIVE_VALUE        : std_logic := '1';
        G_BISS_MAX_DATA_BITS        : integer := 40                                                      -- polarity of reset
    );
    port (
    	CLK                         : in    std_logic;                                          -- system clock
   	 	RST                         : in    std_logic;                                          -- asynchronous reset
   		ENA                         : in    std_logic;                                          -- active high
   	 	START                       : in    std_logic;                                          -- active high start signal - initiate the read process
    	BISS_TYPE 					: in 	std_logic; 														-- biss mode
    	SNIFFER_MODE           : in  std_logic; -- master mode/sniffer moder
    	-- enc signals
    	MA                          : out   std_logic;                                          -- encoder clock
        MA_IN                       : in  std_logic; -- encoder clock for sniffer operation_
   		SLO                         : in    std_logic;                                          -- encoder serial data
    	-- config inputs --			
    	RESOLUTION                  : in    std_logic_vector(5 downto 0);                       -- encoder resolution
   	 	--CRC_WIDTH 					: in 	std_logic_vector(4 downto 0);					    -- encoder crc resolution
   	 	MA_TO_CLK_RATIO             : in    std_logic_vector(7 downto 0);                       -- number of input clocks per MA cycle 
    	ACK_TIMEOUT_DELAY           : in    std_logic_vector(7 downto 0);                       -- number of MA cycle to yield 21us dely 
		END_TIMEOUT_DELAY           : in    std_logic_vector(8 downto 0);                       -- number of MA cycle to yield 40us timeout at end of cycle
		RESET_EN                    : in  std_logic_vector(4 downto 0); -- IN which type of error ,It will enter to reset state (active low)
        RESET_TIME_STATE            : in  std_logic_vector(15 downto 0); -- reset time at us (MA on high)
    	CDM_ADDRESS 				: in 	std_logic_vector(6 downto 0);						-- Master address sent	
    	CDM_OPERATION 				: in 	std_logic_vector(1 downto 0);						-- Read /Write operation 
    	CDM_DATA 					: in 	std_logic_vector(7 downto 0);						-- Master data sent out
   		 -- output to registers/out of the module
   		ENCODER_LIFE_POSITION  		: out 	std_logic_vector(5 downto 0); 
   		ENCODER_POSITION            : out   std_logic_vector(G_BISS_MAX_DATA_BITS-1 downto 0);  -- encoder position
   	 	ENCODER_POSITION_VALID      : out   std_logic;                                          -- active high encoder position valid
    	ENCODER_STATUS              : out   std_logic_vector(7 downto 0);
		CDS_DATA					: out 	std_logic_vector(7 downto 0)
    );
    end component;

    --signals comming out of enc_if to regs and to output from module
    signal enc_if_to_regs_encoder_position          : std_logic_vector(G_BISS_MAX_DATA_BITS-1 downto 0);   -- encoder position
    signal enc_if_to_regs_encoder_position_valid    : std_logic;                                           -- active high encoder position valid
--  signal enc_if_to_regs_encoder_error             : std_logic;                                           -- encoder error
--  signal enc_if_to_regs_crc_error                 : std_logic;                                           -- crc error

    -- signals from regs to config enc_if 
    signal regs_to_enc_if_resolution                : std_logic_vector(5 downto 0);                        -- encoder resolution
    signal regs_to_enc_if_ma_to_clk_ratio           : std_logic_vector(7 downto 0);                        -- number of input clocks per MA cycle 
    signal regs_to_enc_if_ack_timeout_delay         : std_logic_vector(7 downto 0);                        -- number of MA cycle to yield 21us delay 
    signal regs_to_enc_if_end_timeout_delay         : std_logic_vector(8 downto 0);                        -- number of MA cycle to yield 40us delay 
                                                                                                        
    -- signals from top to if
    signal enc_top_to_if_start                      : std_logic;                                           -- start

    -- signals to support mpdule_en - disbaling the module functionality 
    signal top_to_rgs_writedata                     : std_logic_vector(31 downto 0);                       -- writedata
    signal top_to_rgs_write                         : std_logic;                                           -- write
    signal top_to_rgs_byteenable                    : std_logic_vector(3 downto 0);                        -- byteenable
    signal rgs_to_top_waitrequest                   : std_logic;                                           -- waitrequest
    signal rgs_to_top_readdata                      : std_logic_vector(31 downto 0);                       -- readdata
    signal rgs_to_top_readdatavalid                 : std_logic;                                           -- readdatavalid
    signal top_to_rgs_address                       : std_logic_vector(C_BISS_ENC_ADDR_WIDTH-1 downto 0);  -- address
    signal top_to_rgs_read                          : std_logic;                                           -- read

    signal if_to_top_ma                             : std_logic;                                           -- SSI_CLK  
    signal top_to_if_slo                            : std_logic;                                           -- SSI_DATA 

    signal mdl_dis_rddata_valid_reg                 : std_logic;                                           -- readdatavalid
    signal encoder_if_enable                        : std_logic;
    signal encoder_status_sig                       : std_logic_vector(7 downto 0);
    signal enc_top_to_if_biss_interface 			: std_logic; 										   -- biss mode
  --  signal enc_top_top_if_resolution_crc 			: std_logic_vector(4 downto 0);
    signal regs_to_enc_if_cdm_data 					: std_logic_vector(7 downto 0);
    signal regs_to_enc_if_cdm_address 				: std_logic_vector(6 downto 0);
    signal regs_to_enc_if_cdm_operation 			: std_logic_vector(1 downto 0);
    signal enc_if_to_reg_cds_data 					: std_logic_vector(7 downto 0);
    signal enc_if_to_regs_encoder_life_position : std_logic_vector(5 downto 0);
    signal enc_regs_to_if_reset_en : std_logic_vector(4 downto 0);
    signal enc_regs_to_if_reset_time_state : std_logic_vector(15 downto 0);
    signal top_to_if_ma_in : std_logic;
    signal enc_top_to_if_sniffer_mode : std_logic;
    

begin


    regs : biss_enc_regs
    generic map (
        G_RESET_ACTIVE_VALUE        => G_RESET_ACTIVE_VALUE,
        G_CLK_FREQ_IN_MHZ           => G_CLK_FREQ_IN_MHZ
    )
    port map (
        RESET => RESET,
        CLK => CLK,
        -- output to FW 
        ENCODER_LIFE_POSITION => enc_if_to_regs_encoder_life_position,
        ENCODER_POSITION => enc_if_to_regs_encoder_position,
        ENCODER_POSITION_VALID => enc_if_to_regs_encoder_position_valid,
        ENCODER_STATUS => encoder_status_sig,
        CDS_DATA => enc_if_to_reg_cds_data,
        --  FW inputs
        RESOLUTION => regs_to_enc_if_resolution,
        MA_TO_CLK_RATIO => regs_to_enc_if_ma_to_clk_ratio,
        ACK_TIMEOUT_DELAY => regs_to_enc_if_ack_timeout_delay,
        END_TIMEOUT_DELAY => regs_to_enc_if_end_timeout_delay,
        ENCODER_ENA => encoder_if_enable,
        CDM_ADDRESS => regs_to_enc_if_cdm_address,
        CDM_OPERATION => regs_to_enc_if_cdm_operation,
        CDM_DATA => regs_to_enc_if_cdm_data,
        RESET_EN => enc_regs_to_if_reset_en,
        RESET_TIME_STATE => enc_regs_to_if_reset_time_state,
        AV_MM_WRITEDATA => top_to_rgs_writedata,
        AV_MM_WRITE => top_to_rgs_write,
        AV_MM_BYTEENABLE => top_to_rgs_byteenable,
        AV_MM_WAITREQUEST => rgs_to_top_waitrequest,
        AV_MM_READDATA => rgs_to_top_readdata,
        AV_MM_READDATAVALID => rgs_to_top_readdatavalid,
        AV_MM_ADDRESS => top_to_rgs_address,
        AV_MM_READ => top_to_rgs_read
    );


    enc_if : biss_encoder_if
    generic map (
        G_RESET_ACTIVE_VALUE        => G_RESET_ACTIVE_VALUE 
    )
    port map (
        
       
        CLK => CLK,
        RST => RESET,
        ENA => encoder_if_enable,
        START => enc_top_to_if_start,
        BISS_TYPE => enc_top_to_if_biss_interface,
        SNIFFER_MODE => enc_top_to_if_sniffer_mode,
        -- enc signals
        MA => if_to_top_ma,
        MA_IN => top_to_if_ma_in,
        SLO => top_to_if_slo,
        RESOLUTION => regs_to_enc_if_resolution,
        MA_TO_CLK_RATIO => regs_to_enc_if_ma_to_clk_ratio,
        ACK_TIMEOUT_DELAY => regs_to_enc_if_ack_timeout_delay,
        END_TIMEOUT_DELAY => regs_to_enc_if_end_timeout_delay,
        CDM_ADDRESS => regs_to_enc_if_cdm_address,
        CDM_OPERATION => regs_to_enc_if_cdm_operation,
        CDM_DATA => regs_to_enc_if_cdm_data,
        -- output to registers/out of the module
        ENCODER_LIFE_POSITION => enc_if_to_regs_encoder_life_position,
        ENCODER_POSITION => enc_if_to_regs_encoder_position,
        ENCODER_POSITION_VALID => enc_if_to_regs_encoder_position_valid,
        ENCODER_STATUS => encoder_status_sig,
        CDS_DATA => enc_if_to_reg_cds_data,
        RESET_EN => enc_regs_to_if_reset_en,
        RESET_TIME_STATE => enc_regs_to_if_reset_time_state

    );

-----------------------------------------------------------------------------------------------------------
-- module_dis_proc - respond to SW regs access
-----------------------------------------------------------------------------------------------------------
    module_dis_proc: process(CLK , RESET)    
    begin       
        if (RESET = G_RESET_ACTIVE_VALUE ) then
            mdl_dis_rddata_valid_reg <= '0';
        elsif rising_edge (CLK) then
            if (AV_MM_READ = '1') then
                mdl_dis_rddata_valid_reg <= '1';
            else
                mdl_dis_rddata_valid_reg <= '0';
            end if;
        end if;    
    end process module_dis_proc; 

-----------------------------------------------------------------------------------------------------------
--asignments        
-----------------------------------------------------------------------------------------------------------
   -- output encoder values for other modules use (simulink)
    ENCODER_POSITION        <= enc_if_to_regs_encoder_position        when MODULE_EN = '1' else (others =>'0');
    ENCODER_POSITION_VALID  <= enc_if_to_regs_encoder_position_valid  when MODULE_EN = '1' else '0';
-- ENCODER_LIFE_POSITION   <= enc_if_to_regs_encoder_life_position   when MODULE_EN = '1' else (others =>'0');
--  ENCODER_ERROR          <= enc_if_to_regs_encoder_error           when MODULE_EN = '1' else '0';
--  CRC_ERROR              <= enc_if_to_regs_crc_error               when MODULE_EN = '1' else '0';
    ENCODER_STATUS          <= encoder_status_sig when MODULE_EN = '1' else (others=>'0');
    
    -- start signal - checking level rather than rising edge in order to allow consecutive independet of sync suignal , by hard coding START to '1'
    enc_top_to_if_start             <= '1' when START = G_START_POLARITY and MODULE_EN = '1' else '0';
    
    
    -- module_en
    
    top_to_rgs_writedata  <= AV_MM_WRITEDATA           when MODULE_EN = '1' else (others =>'0');    
    top_to_rgs_write      <= AV_MM_WRITE               when MODULE_EN = '1' else '0';
    top_to_rgs_byteenable <= AV_MM_BYTEENABLE          when MODULE_EN = '1' else (others =>'0');
    AV_MM_WAITREQUEST     <= rgs_to_top_waitrequest    when MODULE_EN = '1' else '0';               
    AV_MM_READDATA        <= rgs_to_top_readdata       when MODULE_EN = '1' else (others =>'0');
    AV_MM_READDATAVALID   <= rgs_to_top_readdatavalid  when MODULE_EN = '1' else mdl_dis_rddata_valid_reg;
    top_to_rgs_address    <= AV_MM_ADDRESS             when MODULE_EN = '1' else (others =>'0');
    top_to_rgs_read       <= AV_MM_READ                when MODULE_EN = '1' else '0';
    
    top_to_if_slo         <= SLO                       when MODULE_EN = '1' else '0';
    top_to_if_ma_in       <= MA_IN                     when MODULE_EN = '1' else '0';
    MA                    <= if_to_top_ma              when MODULE_EN = '1' else '0';               

    ENCODER_ENABLED <= encoder_if_enable;
    
    enc_top_to_if_biss_interface <= BISS_TYPE when MODULE_EN = '1' else '0';
    enc_top_to_if_sniffer_mode   <= SNIFFER_MODE when MODULE_EN ='1' else '0';   
       
end biss_encoder_top_arc;
