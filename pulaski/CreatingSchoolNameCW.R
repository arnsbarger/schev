## create division and school name crosswalks
setwd("Google Drive/SCHEV (Peter Blake - Wendy Kang)/")

all<-read.csv("~/Google Drive/SDAL Google Drive Folders/SCHEV (Peter Blake - Wendy Kang)/Code/Bianica/vdoe_hs_total_grade_race.csv",stringsAsFactors = F)

div_num<-c(11,14,124,80,123,91,72)
div_name<-c("BLAND CO PBLC SCHS","BUCHANAN CO PBLC SCHS","ROANOKE CITY PBLC SCHS","ROANOKE CO PBLC SCHS","RICHMOND CITY PBLC SCHS",
             "SUSSEX CO PBLC SCHS","POWHATAN CO PBLC SCHS")
div_cw<-data.frame(div_num,div_name)

div_sch<-unique(select(all,DIV_NUM,DIV_NAME,SCH_NUM,SCH_NAME))
schools<-left_join(div_cw,div_sch,by=c("div_num"="DIV_NUM","div_name"="DIV_NAME"))
sch1<-filter(schools,str_detect(SCH_NAME, "HIGH"))
sch1<-filter(sch1,SCH_NAME!="HIGHLAND PARK ELEM.")
sch1<-filter(sch1,SCH_NAME!="HIGHLAND PARK ELEM")
sch1<-filter(sch1,SCH_NAME!="POWHATAN JR. HIGH")
sch2<-filter(schools,SCH_NAME=="FRANKLIN MILITARY ACADEMY")

sch_cw<-rbind(sch1,sch2)

sch_cw$sch_name_clean<-c("Bland County High","Bland County High","Bland County High",
                         "Council High","Hurley High","Twin Valley High","Grundy High",
                         "Patrick Henry High","William Fleming High",
                         "Cave Spring High","Northside High","Glenvar High","William Byrd High","Hidden Valley High",
                         "Huguenot High","Thomas Jefferson High","Richmond Community High","John Marshall High","George Wythe High",
                         "Armstrong High","Open High",
                         "Sussex Central High","Powhatan High","Franklin Military Academy")

schnums<-unique(sch_cw$SCH_NUM)
schnums<-c("231","61" ,   "260"  , "1000", "1020", "1042" ,"990"  , "390" ,  "400"  , "470",   "520" ,  "610" ,  "630" ,  "754" ,  "1510" ,"20" ,   "452" ,  "730" ,  "741" , 
           "850"  , "90"  ,  "340" ,  "11"  ,  "621"  )
sch_cw$sch_num<-schnums
sch_cw<-select(sch_cw,div_num,div_name,sch_num,sch_names=SCH_NAME,sch_name_clean)

## add column with county name
sch_cw<-read.csv("~/Google Drive/SDAL Google Drive Folders/SCHEV (Peter Blake - Wendy Kang)/Code/Bianica/school_crosswalk.csv",stringsAsFactors = F)
counties<-c("Bland County","Bland County","Bland County","Buchanan County","Buchanan County","Buchanan County","Buchanan County",
            "Roanoke City","Roanoke City","Roanoke County","Roanoke County","Roanoke County","Roanoke County","Roanoke County",
            "Richmond City","Richmond City","Richmond City","Richmond City","Richmond City","Richmond City","Richmond City",
            "Sussex County","Powhatan County","Richmond City")
sch_cw$county_name<-counties

write.csv(sch_cw,"~/Google Drive/SDAL Google Drive Folders/SCHEV (Peter Blake - Wendy Kang)/Code/Bianica/school_crosswalk.csv")
