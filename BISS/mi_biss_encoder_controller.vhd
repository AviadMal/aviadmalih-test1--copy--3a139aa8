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
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

library work;
use work.biss_enc_pkg.all;

entity mi_biss_encoder_controller is
generic (
    G_RESET_ACTIVE_VALUE        : std_logic := '1';                                             -- polarity of reset input
    G_START_POLARITY            : std_logic := '1';                                             -- polarity of start input
    G_LAST_ENCODER_NUM          : integer range 0 to 8-1 := 0;
    G_CLK_FREQ_IN_MHZ           : integer := 1;
    G_BISS_MAX_DATA_BITS        : integer := 40
); 
port    (
    RESET                       : in    std_logic;                                              -- global synchronous reset, polarity defined by G_RESET_ACTIVE_VALUE
    CLK                         : in    std_logic;                                              -- clock input
    MODULE_EN                   : in    std_logic_vector(G_LAST_ENCODER_NUM downto 0);          -- input enable,should be connected to const on top level, cant be changed on the fly.
    SNIFFER_MODE                : in    std_logic_vector(G_LAST_ENCODER_NUM downto 0);         -- master mode/sniffer moder
     
    -- Encoder signals
    MA                          : out   std_logic_vector(G_LAST_ENCODER_NUM downto 0);              -- encoder clock
    MA_IN                       : in    std_logic_vector(G_LAST_ENCODER_NUM downto 0);              -- encoder clock
    SLO                         : in    std_logic_vector(G_LAST_ENCODER_NUM downto 0);              -- encoder serial data
    -- output to other moduls
    START                       : in    std_logic_vector(G_LAST_ENCODER_NUM downto 0);              -- active high start signal - initiate the read process
    BISS_TYPE 					: in 	std_logic_vector(G_LAST_ENCODER_NUM downto 0);              -- '0' mode normal '1' mode safety
--    CRC_WIDTH        			: in 	std_logic_vector ((G_LAST_ENCODER_NUM+1)*5-1 downto 0);		-- crc size from 0 to 16
    ENCODER_POSITION            : out   std_logic_vector((G_BISS_MAX_DATA_BITS*(G_LAST_ENCODER_NUM+1))-1 downto 0);  -- encoder position
    ENCODER_POSITION_VALID      : out   std_logic_vector(G_LAST_ENCODER_NUM downto 0);                        -- active high encoder position valid
    ENCODER_STATUS              : out   std_logic_vector(8*(G_LAST_ENCODER_NUM+1)-1 downto 0);
    ENCODER_ENABLED             : out   std_logic_vector(G_LAST_ENCODER_NUM downto 0);
    --  Avalon_mm slave signals
    AV_MM_WRITEDATA             : in    std_logic_vector(31 downto 0);                          -- writedata
    AV_MM_WRITE                 : in    std_logic;                                              -- write
    AV_MM_BYTEENABLE            : in    std_logic_vector(3 downto 0);                           -- byteenable
    AV_MM_WAITREQUEST           : out   std_logic;                                              -- waitrequest
    AV_MM_READDATA              : out   std_logic_vector(31 downto 0);                          -- readdata
    AV_MM_READDATAVALID         : out   std_logic;                                              -- readdatavalid
    AV_MM_ADDRESS               : in    std_logic_vector(3+C_BISS_ENC_ADDR_WIDTH-1 downto 0);   -- address
    AV_MM_READ                  : in    std_logic                                               -- read
);
end entity;

architecture mi_biss_encoder_top_arc of mi_biss_encoder_controller is

    type slv_array is array (natural range <> ) of std_logic_vector(31 downto 0);
    

    component biss_encoder_top is
    generic (
        G_RESET_ACTIVE_VALUE        : std_logic := '1';                                         -- polarity of reset input
        G_START_POLARITY            : std_logic := '1';                                         -- polarity of start input
        G_CLK_FREQ_IN_MHZ           : integer := 0;
        G_BISS_MAX_DATA_BITS        : integer := 36
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
    end component;
    

    signal av_mm_waitrequest_int        : std_logic_vector(0 to G_LAST_ENCODER_NUM);
    signal instance_select              : integer range 0 to 7;
    signal instance_select_d1           : integer range 0 to 7 := 0;
    signal av_mm_write_int              : std_logic_vector(0 to G_LAST_ENCODER_NUM);
    signal av_mm_read_int               : std_logic_vector(0 to G_LAST_ENCODER_NUM);
    signal av_mm_readdata_int           : slv_array(0 to G_LAST_ENCODER_NUM);
    
begin

    instance_select <=  conv_integer(AV_MM_ADDRESS(AV_MM_ADDRESS'high downto C_BISS_ENC_ADDR_WIDTH));
      

multi_instance: for i in 0 to G_LAST_ENCODER_NUM generate

    biss_encoder_top_inst: biss_encoder_top
    generic map (
        G_RESET_ACTIVE_VALUE        => G_RESET_ACTIVE_VALUE,
        G_START_POLARITY            => G_START_POLARITY,
        G_CLK_FREQ_IN_MHZ           => G_CLK_FREQ_IN_MHZ,
        G_BISS_MAX_DATA_BITS        => G_BISS_MAX_DATA_BITS
    )
    port map (
        SNIFFER_MODE => SNIFFER_MODE(i),
        MA_IN => MA_IN(i),
        RESET => RESET,
        CLK => CLK,
        START => START(i),
        MODULE_EN => MODULE_EN(i),
        MA => MA(i),
        SLO => SLO(i),
        BISS_TYPE => BISS_TYPE(i),
        ENCODER_POSITION => ENCODER_POSITION((i+1)*G_BISS_MAX_DATA_BITS-1 downto i*G_BISS_MAX_DATA_BITS),
        ENCODER_POSITION_VALID => ENCODER_POSITION_VALID(i),
        ENCODER_STATUS => ENCODER_STATUS((i+1)*8-1 downto i*8),
        ENCODER_ENABLED => ENCODER_ENABLED(i),
        AV_MM_WRITEDATA => AV_MM_WRITEDATA,
        AV_MM_WRITE => av_mm_write_int(i),
        AV_MM_BYTEENABLE => AV_MM_BYTEENABLE,
        AV_MM_WAITREQUEST => av_mm_waitrequest_int(i),
        AV_MM_READDATA => av_mm_readdata_int(i),
        AV_MM_READDATAVALID => open, --av_mm_readdatavalid_int(i),
        AV_MM_ADDRESS => AV_MM_ADDRESS(C_BISS_ENC_ADDR_WIDTH-1 downto 0),
        AV_MM_READ => av_mm_read_int(i)
    );
  
    av_mm_write_int(i) <= AV_MM_WRITE when (instance_select = i) else '0';
    av_mm_read_int(i)  <= AV_MM_READ  when (instance_select = i) else '0';
    
    
end generate multi_instance;

    AV_MM_READDATA <= av_mm_readdata_int(instance_select_d1) when instance_select_d1 <= G_LAST_ENCODER_NUM else X"DEADBEEF";

    process(CLK, RESET)
    begin
        if RESET = G_RESET_ACTIVE_VALUE then
            instance_select_d1 <= 0;
            AV_MM_READDATAVALID <= '0';
        elsif rising_edge(CLK) then
            instance_select_d1 <= instance_select;
            AV_MM_READDATAVALID <= AV_MM_READ;--av_mm_read_ff;
        end if;
    end process;
    

    AV_MM_WAITREQUEST <= '0' when av_mm_waitrequest_int = 0 else '1';

end mi_biss_encoder_top_arc;
