------------------------------------------------------------------------------------------------------
--------------------------               BISS_ENC_PKG Package               --------------------------
------------------------------------------------------------------------------------------------------                                                              
LIBRARY ieee;                                                                                       --
USE ieee.std_logic_1164.all;                                                                        --
--use ieee.numeric_std.all;
USE ieee.std_logic_arith.all;                                                                       --
USE ieee.std_logic_unsigned.all;                                                                    --
                                                                                                    --
package BISS_ENC_PKG is                                                                             --

-- Functions declaration: --
function biss_calc_log2 (X: in integer) return integer;

-- types-- 
    type biss_2d_array is array(natural range <> ) of std_logic_vector(31 downto 0);

------------------------------------------------------------------------------------------------------
------------- Constants ------------------------------------------------------------------------------
    constant C_ZEROS                : std_logic_vector(31 downto 0) := X"00000000";
--  constant C_RES_WIDTH            : integer := 6;
--  constant C_RTIO_WIDTH           : integer := 9;
--  constant C_TO_DLY_WIDTH         : integer := 8;
    
    constant C_NUM_OF_CRC_BITS       : integer := 16; -- max resolution is 16 bits
    constant C_NUM_OF_ERROR_BITS     : integer := 1;
    constant C_NUM_OF_WARNING_BITS   : integer := 1;
    constant C_NUM_OF_LIFE_COUNTER   : integer := 6;
    constant C_NUM_OF_ZERO_BITS      :  integer := 1;
    constant C_BISS_MAX_DATA_BITS    : integer := 40; -- max resolution is 40 bits
    constant C_FIXED_DATA_BITS       : integer := C_NUM_OF_ERROR_BITS + C_NUM_OF_WARNING_BITS ;
   -- constant C_FIXED_DATA_BITS_SAFTEY: integer := C_NUM_OF_ZERO_BITS +  C_NUM_OF_ERROR_BITS + C_NUM_OF_WARNING_BITS + C_NUM_OF_CRC_BITS+C_NUM_OF_LIFE_COUNTER;
    constant C_MAX_TOTAL_DATA_BITS   : integer := C_FIXED_DATA_BITS + C_BISS_MAX_DATA_BITS+C_NUM_OF_CRC_BITS+C_NUM_OF_LIFE_COUNTER;
    constant C_SAMPLE_PERIOD         : integer := 2000;--480000;
    constant C_MAX_NUM_OF_ENCODERS   : integer := 4;
    constant C_CRC_WIDTH_NORMAL      : integer := 6;
    constant C_CRC_WIDTH_SAFETY      : integer := 16;
    
    constant C_MAX_TOTAL_MASTER_MESSAGE_BITS : integer := 47;
    constant C_CDS_POSITION : integer := 34;
    

------------------------------------------------------------------------------------------------------       
------------------------------------------------------------------------------------------------------
--    ADDRESSES Constants    -------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------
--  constant C_BISS_ENC_NUM_OF_REGS                  : integer := 4;
--  constant C_BISS_ENC_ADDR_WIDTH                   : integer := biss_calc_log2(C_BISS_ENC_NUM_OF_REGS);
    constant C_BISS_ENC_ADDR_WIDTH                   : integer := 5;-- address is in symboles - i.e jums by 4

    -- addresses are word address -> actual address is word_address*4 
--  constant C_CONFIGURATION_REG_ADDR               : std_logic_vector(C_BISS_ENC_ADDR_WIDTH-1 downto 0):= conv_std_logic_vector(16#0#,C_BISS_ENC_ADDR_WIDTH); --00
--  constant C_GENERAL_REG_ADDR                     : std_logic_vector(C_BISS_ENC_ADDR_WIDTH-1 downto 0):= conv_std_logic_vector(16#1#,C_BISS_ENC_ADDR_WIDTH);

--  constant C_RESOLUTION_ADDR                      : std_logic_vector(C_BISS_ENC_ADDR_WIDTH-1 downto 0):= conv_std_logic_vector(16#0#,C_BISS_ENC_ADDR_WIDTH); --00
--  constant C_CLK_PER_BIT_ADDR                      : std_logic_vector(C_BISS_ENC_ADDR_WIDTH-1 downto 0):= conv_std_logic_vector(16#1#,C_BISS_ENC_ADDR_WIDTH); --04
--  constant C_CLK_FREQ_MHZ_ADDR                     : std_logic_vector(C_BISS_ENC_ADDR_WIDTH-1 downto 0):= conv_std_logic_vector(16#2#,C_BISS_ENC_ADDR_WIDTH); --08
--  constant C_ACK_TO_DLY_ADDR                       : std_logic_vector(C_BISS_ENC_ADDR_WIDTH-1 downto 0):= conv_std_logic_vector(16#3#,C_BISS_ENC_ADDR_WIDTH); --0C
--  constant C_ENC_POSITION_L_ADDR                   : std_logic_vector(C_BISS_ENC_ADDR_WIDTH-1 downto 0):= conv_std_logic_vector(16#2#,C_BISS_ENC_ADDR_WIDTH); --10
--  constant C_ENC_POSITION_H_ADDR                   : std_logic_vector(C_BISS_ENC_ADDR_WIDTH-1 downto 0):= conv_std_logic_vector(16#3#,C_BISS_ENC_ADDR_WIDTH); --14
--  constant C_CRC_ERR_BIT_LOCATION                  : integer := 31; -- location of crc_err bit in ENC_POSITION_H register
--  constant C_ENC_ERR_BIT_LOCATION                  : integer := 30; -- location of enc_err bit in ENC_POSITION_H register

------------------------------------------------&CDS_DATA ------------------------------------------------------  
------------------------------------------------------------------------------------------------------                           

------------------------------------------------------------------------------------------------------
--    Default values         -------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------

--    constant C_RESOLUTION_DEF_VAL                    : std_logic_vector(31 downto 0):= conv_std_logic_vector(16#1A#,32);
--    constant C_CLK_PER_BIT_DEF_VAL                   : std_logic_vector(31 downto 0):= conv_std_logic_vector(16#20#,32);
----  constant C_CLK_FREQ_MHZ_DEF_VAL                  : std_logic_vector(31 downto 0):= conv_std_logic_vector(16#32#,32);
--    constant C_ACK_TO_DLY_DEF_VAL                    : std_logic_vector(31 downto 0):= conv_std_logic_vector(16#21#,32);

------------------------------------------------------------------------------------------------------  

end BISS_ENC_PKG;


package body BISS_ENC_PKG is


    function biss_calc_log2(X	: in integer) return integer is
    variable i	: integer := 0;
    begin
       -- X represents numper of registers, need to figure out the number of log(bits) to represent address.
       while (2**i) < X loop
          i := i+1;
       end loop;		
       return i;
    end biss_calc_log2;

end BISS_ENC_PKG;

