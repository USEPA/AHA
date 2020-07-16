\i /data/transactions_ddl.sql

-- Copy the test data into the table
COPY ACCOUNT_COMPLIANCE_DIM(
    ACCOUNT_NUMBER,
    PRG_CODE,
    OP_YEAR,
    UNITS_AFFECTED,
    ALLOCATED,
    TOTAL_HELD,
    BANKED_HELD,
    CURRENT_HELD,
    COMP_YEAR_EMISS,
    OTHER_DEDUCT,
    CURRENT_DEDUCT,
    DEDUCT_1_1,
    DEDUCT_2_1,
    DEDUCT_286_1,
    TOTAL_DEDUCT,
    CARRIED_OVER,
    EXCESS_EMISS,
    PENALTY_AMOUNT,
    PENALTY_DEDUCT_INFO,
    TOTAL_REQ_DEDUCT,
    ADD_DATE,
    USERID,
    DATA_SOURCE        
  )
FROM '/data/account_compliance_dim.csv' DELIMITER ',' CSV HEADER;

COPY ACCOUNT_FACT(
 ACCOUNT_NUMBER,
 PRG_CODE,
 ACCOUNT_NAME,
 ACCOUNT_TYPE,         
 UNIT_ID,
 UNITID,
 OP_STATUS_INFO,
 PRIMARY_FUEL_INFO,    
 SECONDARY_FUEL_INFO,
 OWN_DISPLAY,
 UNIT_TYPE_INFO,
 PRM_DISPLAY_NAME,
 PRM_DISPLAY_BLOCK,
 ALT_DISPLAY_NAME,
 ALT_DISPLAY_BLOCK,
 EPA_REGION,           
EPA_REGION_DESCRIPTION,
STATE,
STATE_NAME,
SOURCE_CAT,           
FAC_ID,
FACILITY_NAME,
ORISPL_CODE,
PHASE1_ALLOCATION,
PHASE2A_ALLOCATION,
PHASE2B_ALLOCATION,
DATA_SOURCE,
ADD_DATE,
USERID,
SO2_CONTROL_INFO,
NOX_CONTROL_INFO,
PART_CONTROL_INFO,     
NERC_REGION,
NERC_DESCRIPTION,
ACCOUNT_TYPE_CODE,
OP_STATUS 
  )
FROM '/data/account_fact.csv' DELIMITER ',' CSV HEADER;

COPY TRANSACTION_FACT
(
 TRANSACTION_ID,
 PRG_CODE,
 TRANSACTION_DATE,
 TRANSACTION_TYPE_CODE,
 TRANSACTION_TYPE,
 TRANSACTION_TOTAL,
 BUY_ACCT_NUMBER,
 BUY_PPL_ID ,
 BUY_DISPLAY_NAME,
 BUY_DISPLAY_BLOCK,
 BUY_OWN_DISPLAY_NAME,
 BUY_SOURCE_CAT,       
BUY_EPA_REGION,
BUY_STATE,
BUY_ORISPL_CODE,
BUY_FACILITY_NAME,
SELL_ACCT_NUMBER,
SELL_PPL_ID,
SELL_EPA_REGION,
SELL_SOURCE_CAT,
SELL_STATE,
SELL_ORISPL_CODE,
SELL_FACILITY_NAME,
SELL_DISPLAY_NAME,
SELL_DISPLAY_BLOCK,
SELL_OWN_DISPLAY_NAME,
DATA_SOURCE,
USERID,
ADD_DATE,
BUY_FAC_ID,
SELL_FAC_ID,
BUY_ACCT_NAME,
SELL_ACCT_NAME,
BUY_FULL_NAME,
SELL_FULL_NAME,
BUY_NERC_REGION,
SELL_NERC_REGION,
BUY_ACCOUNT_TYPE,
SELL_ACCOUNT_TYPE, 
BUY_ACCOUNT_TYPE_CODE,
SELL_ACCOUNT_TYPE_CODE,
BUY_UNIT_ID,         
SELL_UNIT_ID      
)
FROM '/data/transaction_fact.csv' DELIMITER ',' CSV HEADER;

COPY FACILITY_SS
(
FAC_ID,                                                                          
ORIS_CODE,                                                                      
FACILITY_NAME,                                                                   
DESCRIPTION,                                                                     
STATE,                                                                           
COUNTY_CD,                                                                       
SIC_CODE,                                                                        
EPA_REGION,                                                                      
NERC_REGION,                                                                     
AIRSID,                                                                          
FINDSID,                                                                         
STATEID,                                                                         
LATITUDE,                                                                        
LONGITUDE,                                                                       
USERID,                                                                          
ADD_DATE,                                                                        
UPDATE_DATE,                                                                     
FRS_ID,                                                                          
PAYEE_ID,                                                                        
PERMIT_EXP_DATE,                                                                 
LATLON_SOURCE,
TRIBAL_LAND_CD
)
FROM '/data/facility_ss.csv' DELIMITER ',' CSV HEADER;


COPY UNIT_FACT
(
  UNIT_ID,         
 OP_YEAR,                  
 FAC_ID ,                  
 FACILITY_NAME ,             
 ORISPL_CODE ,                
 UNITID ,                
 COUNTY_CODE  ,                 
 COUNTY  ,               
 FIPS_CODE  ,              
 COMM_OP_DATE     ,                    
 COMR_OP_DATE        ,                     
 SOURCE_CAT       ,             
 CAPACITY_INPUT    ,              
 CAPACITY_OUTPUT   ,                
 STATE     ,                  
 STATE_NAME     ,             
 LATITUDE     ,                 
 LONGITUDE  ,                 
 EPA_REGION,                 
 EPA_REGION_DESCRIPTION,             
 NAICS_CODE,                
 NAIC_CODE_DESCRIPTION,          
 SIC_CODE,               
 SIC_CODE_DESCRIPTION ,          
 NERC_REGION,              
 NERC_DESCRIPTION,             
 PRG_CODE_INFO,             
 OP_STATUS_INFO,        
 PRIMARY_FUEL_INFO,              
 SECONDARY_FUEL_INFO,          
 UNIT_TYPE_INFO,             
 DATA_SOURCE,        
 ADD_DATE,                   
 USERID,           
 SO2_CONTROL_INFO,          
 NOX_CONTROL_INFO,            
 PART_CONTROL_INFO,            
 NOX_PHASE,            
 SO2_PHASE,          
 ASSOC_STACKS
)
FROM '/data/unit_fact.csv' DELIMITER ',' CSV HEADER;

COPY OZONE_UNIT_DATA
(
  UNIT_ID,                                                                         
OP_YEAR,                                                                         
COUNT_OP_TIME,                                                                   
SUM_OP_TIME,                                                                     
GLOAD,                                                                           
SLOAD,                                                                           
TLOAD,                                                                           
HEAT_INPUT,                                                                      
SO2_MASS,                                                                        
SO2_MASS_LBS,                                                                    
SO2_RATE,                                                                        
SO2_RATE_SUM,                                                                    
SO2_RATE_COUNT,                                                                  
CO2_MASS,                                                                        
CO2_RATE,                                                                        
CO2_RATE_SUM,                                                                    
CO2_RATE_COUNT,                                                                  
NOX_MASS,                                                                        
NOX_MASS_LBS,                                                                    
NOX_RATE,                                                                        
NOX_RATE_SUM,
NOX_RATE_COUNT,                                                                  
NUM_MONTHS_REPORTED,                                                             
DATA_SOURCE,                                                                     
USERID,                                                                          
ADD_DATE             
)
FROM '/data/ozone_unit_data.csv' DELIMITER ',' CSV HEADER;

COPY ANNUAL_UNIT_DATA
(
UNIT_ID,                                                                         
OP_YEAR,                                                                         
COUNT_OP_TIME,                                                                   
SUM_OP_TIME,                                                                     
GLOAD,                                                                           
SLOAD,                                                                           
TLOAD,                                                                           
HEAT_INPUT,                                                                      
SO2_MASS,                                                                        
SO2_MASS_LBS,                                                                    
SO2_RATE,                                                                        
SO2_RATE_SUM,                                                                    
SO2_RATE_COUNT,                                                                  
CO2_MASS,                                                                        
CO2_RATE,                                                                        
CO2_RATE_SUM,                                                                    
CO2_RATE_COUNT,                                                                  
NOX_MASS,                                                                        
NOX_MASS_LBS,                                                                    
NOX_RATE,                                                                        
NOX_RATE_SUM,  
NOX_RATE_COUNT,                                                                  
NUM_MONTHS_REPORTED,                                                             
DATA_SOURCE,                                                                     
USERID,                                                                          
ADD_DATE 
)
FROM '/data/annual_unit_data.csv' DELIMITER ',' CSV HEADER;

COPY PROGRAM_YEAR_DIM
(
 UNIT_ID,                  
 PRG_CODE,                
 PROGRAM_DESCRIPTION ,             
 OP_YEAR,                
 REPORT_FREQ ,                 
 DATA_SOURCE ,             
 USERID ,                
 ADD_DATE ,                      
 NON_EGU_FLG,             
 COMPLIANCE_IND 
)
FROM '/data/program_year_dim.csv' DELIMITER ',' CSV HEADER;

COPY TRANSACTION_BLOCK_DIM
(
  TRANSACTION_BLOCK_ID ,              
 TRANSACTION_ID,              
 START_BLOCK ,                
 END_BLOCK ,                  
 ORIGINATING_ACCT_NUMBER ,                
 ORIGINATING_PARTY,                
 TOTAL_BLOCK  ,                  
 VINTAGE_YEAR,                   
 DATA_SOURCE,                
 USERID   ,                
 ADD_DATE ,                        
 PRG_CODE  ,              
 BLOCK_PRG_CODE   ,                 
 ALLOWANCE_TYPE       ,               
 ALLOWANCE_TYPE_CODE                     
)
FROM '/data/transaction_block_dim.csv' DELIMITER ',' CSV HEADER;

COPY OP_STATUS_YEAR_DIM
(
 UNIT_ID  ,                  
 OP_YEAR   ,                   
 OP_STATUS           ,                 
 OP_STATUS_DESCRIPTION        ,                
 DATA_SOURCE                ,               
 USERID                    ,                
 ADD_DATE                            
)
FROM '/data/op_status_year_dim.csv' DELIMITER ',' CSV HEADER;