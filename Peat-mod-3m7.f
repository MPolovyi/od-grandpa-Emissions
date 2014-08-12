      use FileReading
      use FormattedOutput
c      =========================================================================
c             M O D E L   D I N A M I K I   O R G A N I C H N O I   R E C H O V I N I 
c            V   O R G A N I C H N I X   G R U N T A X (T O R F O V I S C H A X)   T A 
c            V I K I D I V   V U G L E Z U,  M E T A N U   i   Z A K I S U   A Z O T U 
c                              Z   Z I X    G R U N T I V   (Peat-GHG-Model)    


             
c==========================================================================
      integer, dimension(36) :: gim
      real, dimension(9) :: a9
      real, dimension(36) :: j1m,ts11,epot,filt, ossrnz,srdww,osnz,rstek,dw41,
     > dw51,CperW1,CperW2,
     > RPM,DPM,BIO1,HUM1,IOM,CUrOst, raC,TSMD,MTSMD,rbC,rcC,Tpoch,TIsp, rd51,rd42,HUM2,BIO2,DBIO,DHUM,DCO2,
     >  R1CO2,R2CO2,ratX,rE,rMCsoi, rMNsoi, BIO3,HUM3,R3CO,DPM0,RPM0,SDPM,SRPM, SBIO,SHUM,SDPM0,SRPM0,SBIO0,SHUM0,
     > RIOM,SDBIO,SDHUM,SDCO,SBIO1,SHUM1, SR1CO,SBIO2,SHUM2,SR2CO,SBIO3,SHUM3, SR3CO,FDPM,FRPM,FHUM,SHUM00,
     > FDBIO,FDHUM,FDCO,FBIO1,FHUM1, FR1CO,FBIO3,FHUM3,FR3CO, rd1,rd2,rd3,rd4,rd5,rd6,rd7,rd8,
     > rd9,rd10,rd11,rd12, rds1,rds2,rds3,rds4,rds5,rds6,rds7, rds8,rds9,rds10,rds11,rds12, rdf1,rdf2,rdf3,rdf4,rdf5,rdf6,rdf7,
     > rdf8,rdf9,rdg1,rdg2, rdg3,rdg4,rdg5,rdg6,rdg7,rdg8, rdg9,rdg10,rdg11,rdg12,rdg13,rdg14,
     > rdh1,rdh2,rdh3,rdh4,rmg1,rmg2, rmgw1,RDPM,RRPM,rmgw2,rmgw3,rmgw4, rmgw5,rchW2,pBIO,pHUM,pCO2, PrRPM,PrDPM,PrBIO1,PrHUM1,
     > PrHUM2,PrBIO2,PrDBIO,PrDHUM,PrDCO2,  PrR1CO,PrR2CO,Prrdh1,Prrdh2,Prrdh3, Prrdh4,
     > PrBIO3,PrHUM3,PrR3CO,PrDPM0,PrRPM0, BIOrst,BIOsoi,BIOfum,HUMrst,HUMsoi,
     > HUMfum,CO2rst,CO2soi,CO2fum,PoleC, PolCO2,rgdmn1, Whgr,rmW,rmpH, rmT1,rmW1,rmpH1,
     > rxrd1,rxrd2,CHBIO1,CHHUM1,CHBIO2,CHHUM2, CHBIO3,CHHUM3,HSBIO,HHUM,HSBIO1,HHUM1,
     > HSBIO2,HHUM2,HSBIO3,HHUM3,CHBIO,CHHUM, HSBIO0,HHUM0,rch1,rch2,HHUM00,rch3,
     > rch4,rch5,rch6,rchW1, PCHBIO,PCHHUM,PCHB1,PCHH1,PCHB2,PCHH2,
     > PCHB3,PCHH3,CHBIso,CHHUso,CHsoil,CHrst, CHBIrs,CHHUrs,FDPM0,FRPM0,FHUM0,CHFBI0,
     > CHFHU0,CHFBI1,CHFHU1,CHFBI3,CHFHU3, CHBIfm,CHHUfm,CHfum,drww1,prCOrs, rmt2,rmW2,rmpH2,SMCrst,rCrst,
     > SMCsoi,SMCfum,rCsoil,rCfum,SMNrst,rNrst, SMNsoi,SMNfum,rNsoil,rNfum,CNrst,CNsoil,
     > CNfum,CNsum,VNnitr,VNN2O,VNNO,denNO3, denW,denCO2,denpW,dnpNO3,VNdeni,VNdN2,
     > VNdN2O,VNvfum,VNvfrt,Winfil,RNinfl, Nimmob,rdCHrs,rdCHso,rdCHfm,rdRsCH,
     > RNupt,OtnUrs,rdSoCH,rdFmCH,summMetanMonths, rOCNrs,rdNH4,rdNO3,rdNN20,BNNH4,BNNO3,
     > rNmicr,FdWFPS,FdNO3,FdCO2,FdSCO2, gn1,gn2,gn3,gn4,gn5,gn6,gn7,
     > rmW3, ts2m

      real, dimension(366) :: tss

      real :: sumOs,SmDBIO,SmBIO1,SmBIO2,SmBIO3,SmDHUM,SmHUM1,SmHUM2,
     > SmHUM3,SmDCO,SmR1CO,SmR2CO,SmR3CO,SsDBIO,SsBIO1,SsBIO2,SsBIO3,
     > SsDHUM,SsHUM1,SsHUM2,SsHUM3,SsDCO,SsR1CO,SsR2CO,SsR3CO,
     > SFDBIO, SFBIO1,SFBIO3,SFDHUM,SFHUM1,SFHUM3,SFDCO,SFR1CO,
     > SFR3CO,RtBIO,RtHUM,RtCO,RslBIO,RslHUM,RslCO,RFBIO,RFHUM,
     > RFCO, SumBIO,SumHUM,SumCO,BalanC,SSSHUM,
     > PerzW1,PerzW2,NorW1,NorW2,RRSHUM,UR,SumRst,prSMCO,
     > RKWin,PKpoch,PKcult,RWin,SumDPM,SumRPM,RazDPM,RazRPM,
     > TSMCrs,TSMCso,TSMCfm,RNNH4,RNNO3,TSMNrs,TSMNso,TSMNfm,TMNN2O,
     > SMCHrs,SMCHso,SMCHfm,rastCH,soilCH,fumCH,SummarnoeWidilenieMetana,OCNrst,
     > SnN2O,SdN2O,SN2O,SMCOrs,SMCOso,SMCOfm,SPolCO
      
      integer j

      integer gi,g2,rd41,b1,b2,b3

      integer, parameter :: OutputFileUnit = 106

      integer :: n, t0, n1, n2
      real :: fi, urostCoeff, tmpVal, tmpVal1, tmpVal2, tmpVal3, tmpVal4, tmpVal5
      integer, dimension(:), allocatable :: dv
      real, dimension(:), allocatable :: ts, os, dww, usl2, usl3, usl4, inf, rnitr, hgr
      real, dimension(36) :: tmpArray2, tmpArray, dekadesReal, gimReal
      

c      open (unit=InputFileUnit, file="Peat-mod-3.dat",status="old",form="formatted")
      Open (UNIT=OutputFileUnit,FILE="Peat-mod-main-3m7.res")
      Open (UNIT=7,FILE="Peat-mod-3m7.res")
      Open (UNIT=8,FILE="Peat-mod-omm7.res")



      call ReadDataFromFile("Peat-mod-3.dat", Int1=n, Int2=t0, Int3=n1, Int4=n2, 
     >      Real1=fi, IntArrOne1=dv, 
     >      RealArrOne1=ts, RealArrOne2=os, RealArrOne3=dww, RealArrOne4=usl2, RealArrOne5=usl3,
     >      RealArrOne6=usl4, RealArrOne7=inf, RealArrOne8=rnitr, RealArrOne9=hgr)

      i = 1
      if(i.ne.1) inf(8)=BalanC
      if(i.ne.1) inf(1)=SumDPM
      
 4118 format(4x,76("*"))
 4117 format(10x,"MODEL  DINAMIKI  ORGANICHNOI  RECHOVINI") 
 4120 format(10x,"V  ORGANICHNIX  GRUNTAX(TORFOVISCHAX)  TA") 
 4813 format(10x,"VIKIDIV  VUGLEZU, METANU  i  ZAKISU  AZOTU") 
 4119 format(10x,"        Z  ZIX   GRUNTIV  (Peat-GHG-Model)  ")    




c 4117 format(10x,"M O D E L  N A K O P L E N I J  U G L E R O D A")
c 4120 format(10x,"       V  P O C H V E   ")
c 4813 format(10x," MODIFIZIROVANNIY VARIANT ROTHC-26.3  modeli     ") 
c 4119 format(4x,76("*"))
 4193 format(4x,76("*"))
  117 format(10x,"      W X O D N A J   I N F O R M A Z I J ")
  118 format(1x,76("-"))
  119 format(1x,76("*"))
  120 format(4x,76("-"))
  121 format(" ")
  129 format(1x,76("-"))
      write(OutputFileUnit,4118)
      write(OutputFileUnit,4117)
      write(OutputFileUnit,4120)
      write(OutputFileUnit,4813)
      write(OutputFileUnit,4119)
      write(OutputFileUnit,117)
      write(OutputFileUnit,4193)
      write(OutputFileUnit,116) a55
      write(OutputFileUnit,116) a55
      write(OutputFileUnit,116) a55
      write(OutputFileUnit,116) a1,a2,a3,a4
      write(OutputFileUnit,100) n,t0,n1,n2,fi
      write(OutputFileUnit, *)" Srednjj za mesjz tempsratura vozduxa (grad. C):"
      write(OutputFileUnit,102) (ts(j),j=1,n)
      write(OutputFileUnit, *) "      Summa osadkov za mesjz (mm):"
      write(OutputFileUnit,102) (os(j),j=1,n)
      write(OutputFileUnit, *) " Srednjj za mesjz otnositelnaj vlagnost vozduxa (%):"
      write(OutputFileUnit,102) (dww(j),j=1,n)
      write(OutputFileUnit, *) " Uslovnaj velichina 2 (nomer mesjza goda):"
      write(OutputFileUnit,102) (usl2(j),j=1,n)
      write(OutputFileUnit, *) "Chislo dekad v mesjze, kogda uroven gruntovix vod)"
      write(OutputFileUnit, *) " raven i menee 20 sm)"

      write(OutputFileUnit,102) (usl3(j),j=1,n)

      write(OutputFileUnit, *) " Uslovnaj velichina(mesjzi vegetazii kulturi):"
      write(OutputFileUnit,102) (usl4(j),j=1,n)
      write(OutputFileUnit, *) "     Chislo dney v raschetnom mesjze :"
      write(OutputFileUnit,115) (dv(j),j=1,n)
      write(OutputFileUnit,118)
      write(OutputFileUnit, *) "  M A S S I V  ' I N F ' - parametri modeli   :"
      write(OutputFileUnit,101)(inf(j),j=1,21)
      write(OutputFileUnit, *) "  M A S S I V  ' Rnitr ' - parametri modeli   :"
      write(OutputFileUnit,101)(rnitr(j),j=1,15)
      write(OutputFileUnit, *) " Sredniy za mesjz uroven gruntovix vod (sm):"
      write(OutputFileUnit,"(12f7.1)") (hgr(j),j=1,n)


c==========================================================================
c       O P I S A N I E   V X O D N O Y   I N F O R M A Z I I 
c--------------------------------------------------------------------------
c   n -chislo raschetnix  mesjzev
c   reserv
c   ravno 1
c   ravno 1
c   n2-mesjz vsxodov (1-janvar,2-fevral,3-mart)
c   fi-schirota punkta(gradusi s desjtimi)
c    ts(j)-srednie za mesjz temperatura vozduxa (qradusi C)
c    os(j)-summa osadkov za mesjz (mm)
c    dww(j)-srednjj za mesjz otnositelnaj vlagnost vozduxa (%)
c    usl2(j)-Uslovnaj velichina (nomer mesjza goda):
c    usl3(j)-Chislo dekad v mesjze, kogda uroven gruntovix vod raven i menee 20 sm)
c    usl4(j)- Uslovnaj velichina(period vegetazii kulturi):
c    dv(j)-chislo dney v raschetnom mesjze
c==========================================================================
c             O P I S A N I E    M A S S I V A     " I N F "  
c==========================================================================

c  inf(1)-Nerazlogivshiesj ostatki predidushego goda,t/ga
c     inf(2)- poristost pochvi, otn.ed.
c inf(3) - % glini v pochve
c inf(4)  -  kod s-x kulturi: 1 - ozim pscheniza, 2 - jachmen  3 - gorox
c                         4 - kukuruza na zerno  5 - kukuruza na silos
c                         6 - podsolnechnic  7 - ljuzerna, 0  - par
c inf(5) - chislo mesjzev vegetazii
c   inf(6) - urogay osnovnoy produkzii, tonn/ gektar (t/ga)
c    inf(7) - nomer 1-go mesjza vegetazii kulturi
c inf(8) - visota (tolscina) sloj pochvi, m
c inf(9) - Organicheskie  udobrenij,  t/ga
c       write(OutputFileUnit, *) "inf(10) - kod prirodnoy rastitelnosti:
c         1- vologi luki z dominuvannjm  Deschampsia caespitosa ,
c         2- vologi luki z dominuvannjm  Molinia caerulea ,
c         3 ugrupuvannj gjorstkix bezlistix zlakovidnix  rasteniy tipa  Cyperaceae та Juncaceae,
c         4 ugrupuvannj  nizkix kornevischnix i nizkokupinnix osok   ,   то    ;
c         5 ugrupuvannj  visokix kupinnix osok
c         6 zarosli visokotravnix kornevischnix  geliofitov (ocheret,  rogoz)
c         7 verba v vozraste do 7 let
c         8 verba v vozraste 8-10 let
c         9 verba v vozraste  10-12 let
c        10 verba v vozraste bolee 12 let     

c     write(OutputFileUnit, *) "inf(11) - massa nadzemnoy chasti prirodnoy rastitelnosti,  t/ga"
c       write(OutputFileUnit, *) "inf(12) - dolj otmershey nadzemnoy chasti (trav ili listev dereva),  otn. ed."
c       write(OutputFileUnit, *) "inf(13) - otnoshenie nadzemnoy k podzemnoy chasti rasteniy ,  otn.ed."
c       write(OutputFileUnit, *) "inf(14) - dolj otmershix korney,  otn.ed."
c       write(OutputFileUnit, *) "inf(15) - wlgjnost zavjdanij,  mm"
c        write(OutputFileUnit, *) "inf(16) - naimenshaj wlagoemkost,  mm"
c        write(OutputFileUnit, *) "inf(17) - wlagoemkost nasishenij,  mm"
c        write(OutputFileUnit, *) "inf(18) - pH pochvi,  otn.ed."
c        write(OutputFileUnit, *) "inf(19) - objemnaj massa pochvi,  g/cm3."
c        write(OutputFileUnit, *) "inf(20) - organicheskoe veschestvo pochvi (C), %
c        write(OutputFileUnit, *) "inf(21) - kolichestvo kustov verboloza ili kolichestvo derevev (verbi) na 1 ga, shtuk /ga
c==========================================================================
c             O P I S A N I E    M A S S I V A     " Rnitr "  
c==========================================================================

c        write(OutputFileUnit, *) "rnitr(1) - soderganie ammonij NH4 v pochvi, mg N / 100 g pochvi 
c        write(OutputFileUnit, *) "rnitr(2) - soderganie nitratov NO3 v pochvi, mg N / 100 g pochvi
c        write(OutputFileUnit, *) "rnitr(3) - otnoshenie C/N dlj rastitelnix ostatkov, otn.ed. 
c        write(OutputFileUnit, *) "rnitr(4) - otnoshenie C/N dlj pochvi, otn.ed.
c        write(OutputFileUnit, *) "rnitr(5) - otnoshenie C/N dlj udobreniy, otn.ed. 
c        write(OutputFileUnit, *) "rnitr(6) - otnoshenie C/N  summarnoe - obobschennoe, otn.ed.
c       write(OutputFileUnit, *) "rnitr(7) - kolichestvo vnesennogo azotnogo udobrenij,kg N / ga
c        write(OutputFileUnit, *) "rnitr(8) - dolj N v azotnom udobrenii, otn. od.
c        write(OutputFileUnit, *) "rnitr(9) - kriticheskoe kolichestvo osadkov, pri kotorom 
c                             nachinaetsj vivetrivanie ammonij
c       write(OutputFileUnit, *) "rnitr(10) - dolj ammoniju v pogloschennom rasteniem  azote
c       write(OutputFileUnit, *) "rnitr(11) - dolj nitraotv  v pogloschennom rasteniem  azote
c===========================================================================
c  NACHALNIe DANNIe KOMPONENTOV ORGANICHNOGO MATERIALA   POCHVI
c================================================================
c        SDPM(j)=0.004527*inf(8)
c        SRPM(j)=0.1324*inf(8)
c        SBIO(j)=0.0197*inf(8)
c        SHUM(j)=0.7636*inf(8)

c       write(OutputFileUnit, *) "rnitr(12) -OMSDPM(j) 
c       write(OutputFileUnit, *) "rnitr(13) -OMSRPM(j) 
c       write(OutputFileUnit, *) "rnitr(14) -OMSBIO(j) 
c       write(OutputFileUnit, *) "rnitr(15) -OMSHUM(j) 
c==========================================================================
      write(OutputFileUnit,129)
      write(OutputFileUnit,119)
      write(OutputFileUnit,122)
  122 format(10x,"     R E S U L T A T     R A S C H E T O V       ")
      write(OutputFileUnit,119)

  100 format(4i3,f6.2)
  101 format(9f8.3)
  102 format(14f5.1)
  103 format(9f8.3)
  115 format(24i3)
  116 format(4a4)
 1141 format(4a20)
 
      j1=1
      gi=0
      ts2=0
      j2=0
      SumOs=0
      rd41=0

      write(OutputFileUnit,120)
c++++++++++++++

      SmDBIO=0
      SmBIO1=0
      SmBIO2=0
      SmBIO3=0
      SmDHUM=0
      SmHUM1=0
      SmHUM2=0
      SmHUM3=0
      SmDCO=0
      SmR1CO=0
      SmR2CO=0
      SmR3CO=0
c+++++++++++++++++

      SsDBIO=0
      SsBIO1=0
      SsBIO2=0
      SsBIO3=0
      SsDHUM=0
      SsHUM1=0
      SsHUM2=0
      SsHUM3=0
      SsDCO=0
      SsR1CO=0
      SsR2CO=0
      SsR3CO=0
	   SMCOrs=0
      SMCOso=0
      SMCOfm=0
c+++++++++++++++++

      SFDBIO=0
      SFBIO1=0

      SFBIO3=0
      SFDHUM=0
      SFHUM1=0

      SFHUM3=0
      SFDCO=0
      SFR1CO=0

      SFR3CO=0
      prSMCO=0 
c+++++++++++++++++
      RtBIO=0
      RtHUM=0
      RtCO=0
      RslBIO=0
      RslHUM=0
      RslCO=0
      RFBIO=0
      RFHUM=0
      RFCO=0
      SumBIO=0
      SumHUM=0
      SumCO=0
      RRSHUM=0
      RazDPM=0
      RazRPM=0
c++++++++++++++++++++++++++++++++++
      TSMCrs=0
      TSMCso=0
      TSMCfm=0
      TSMNrs=0
      TSMNso=0
      TSMNfm=0
      TMNN2O=0 
      SnN2O=0
      SdN2O=0   
      SN2O=0 

	   RNNH4=0
      RNNO3=0

      SMCHrs=0
      SMCHso=0
      SMCHfm=0
      rastCH=0
      soilCH=0
      fumCH=0
      SummarnoeWidilenieMetana=0  
c++++++++++++++++++++++++++++++++++
  331 format(1x,4f7.3)
      write(OutputFileUnit,121)
      
      do j=1,n
         nn=dv(j)
         do l=1,nn
            ts1=ts(j)-inf(1)
            
            if(ts1 .lt. 0) then
               ts1=0
            end if

            ts2=ts2+ts1
            tss(l+j2)=ts2
         end do
         j2=j2+dv(j)
         ts11(j)=ts1
      end do
      
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!     
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      do j=1,n
         urostCoeff = urostCoeff + (2.3026*(2./3)*10.**(2.-(2./3)*((j-inf(7)+1)/3)))/(1.+10.**(2.-(2./3)*((j-inf(7)+1)/3)))**2
      end do


      do j=1,n
         s1=0
         s2=0
         s3=0
         s4=0
         s5=0
         s6=0
         ts1=ts11(j)
  334    format(1x,f10.2)
         nn=dv(j)
         do k1=1,nn
            ts2=tss(gi+1)
            gi=gi+1
         end do

c==========================================================
c  WODNIY BLOK, RASCHET DEFIZITA WLAGI V POCHVE
c=============================================================
c    rd51 - max TSMD,  inf(3)  - prozent glini v pochve
         rd51(j)=-(20.0+1.3*inf(3)-0.01*(inf(3))**2)
cccccc        ratX(j)=1.67*(1.85+1.60*exp(-0.0786*inf(3)))

         ratX(j)=3.51*(1.85+1.60*exp(-0.0786*inf(3)))

c  velichina     1/ratX(j) budet velichina E v modeli ECOSSE
         rE(j)=1/ratX(j)

         pBIO(j)=rE(j)-(0.85/(1+rE(j)))
         pHUM(j)=0.85/(1+rE(j))
         pCO2(j)=1-rE(j)  


c  Tisp -isparjemost po Ivanovu
         TIsp(j)=(0.0018*((25+ts(j))**2)*(100-dww(j)))
c dww - otnositelnaj vlagnost vozduxa

c   TSMD - accomulated Topsoil Moisture Deficit
         TSMD(j)=Os(j)-0.75*TIsp(j)
         if(os(j).gt.TIsp(j)) TSMD(j)=0
         if(TSMD(j).lt.rd51(j))TSMD(j)=rd51(j)
         rd41=rd41+tsmd(j)
c  rd41 - AccTSMD
         if(os(j).gt.TIsp(j)) rd41=0
         if(rd41.lt.rd51(j))rd41=rd51(j)+0.01
c=============================================================
c===============================================+++++++++++
         Whgr(j) = -1.5643*(-1*hgr(j))+452.32
     
         if((Whgr(j)-inf(15)).lt.(inf(16)-inf(15))/2) then
            rmW(j)= 0.2+(0.8*2/(inf(16)-Inf(15)))*(Whgr(j)-inf(15))
         end if


         if((Whgr(j)-inf(15)).gt.(inf(16)-inf(15))/2.and.(Whgr(j)-inf(15)).lt.(inf(16)-inf(15)))   then
            rmW(j)=1.0
         end if
     


         if((Whgr(j)-inf(15)).gt.(inf(16)-inf(15))) then
            rmW(j)= 1-(0.8/((inf(17)-inf(15))-(inf(16)-inf(15))))*((Whgr(j)-inf(15))-(inf(16)-inf(15)))
         end if
     
c        if(Whgr(j).gt.inf(16)) rmW(j)= 0.8-(0.8/(inf(17)-inf(16)))*
c     6   (Whgr(j)-(inf(16)))

ccccccccccccccc	rchW1(j)=(Whgr(j)-inf(15))/(inf(16)-inf(15))

         rchW1(j)=Whgr(j)/inf(16)

         tmpVal  = inf(17)/inf(16)
         tmpVal1 = 381.2*(tmpVal**4)-1643.3*(tmpVal**3)+2658.9*(tmpVal**2)-1913*tmpVal+516.21

         tmpVal2 = (381.2*tmpVal**4-1643.3*tmpVal**3+2658.9*tmpVal**2-1913*tmpVal+516.21)
         tmpVal4 = tmpVal

         if(Whgr(j).lt.(inf(16))) then
            rmW1(j)=0
         end if
         if (Whgr(j).gt.(inf(16))) then
            rmW1(j)=381.2*rchW1(j)**4-1643.3*rchW1(j)**3+2658.9*rchW1(j)**2-1913*rchW1(j)+516.21
         end if
         if (Whgr(j) .gt. inf(17)) then
            tmpVal  = (-1.5643*(-1*hgr(j))+452.32)/inf(16)
            tmpVal3 = (381.2*rchW1(j)**4-1643.3*rchW1(j)**3+2658.9*rchW1(j)**2-1913*rchW1(j)+516.21)
            rmW1(j) = (20*(tmpVal - tmpVal4))**5 + (tmpVal3 - tmpVal2) + tmpVal1
         end if


ccc       if(rmW1(j).gt.1) rmW1(j)=1
cc     6   -inf(15))))/(inf(17)-(inf(16)-inf(15)))


         rmpH(j)=0.2+(1-0.2)*((inf(18)-2)/(5-2))
         if(Whgr(j).gt.inf(16)) rmpH(j)=((1.0**(1/(-50))+exp((-1)*inf(18))))**(-50) 
c==================================================
ccccccccc       rmT1(j)=47.9/(1+exp(106/(Tpoch(j)+18.3)))
c       rmW1(j)=0.5
         rmpH1(j)=((1.0**(1/(-50))+exp((-1)*inf(18))))**(-50)

         rmW3(j) = -0.0012*hgr(j)**2 + 0.002*hgr(j) + 1.006


c============================================================
c  RASCHET TEMPERATURI POCHVI NA GLUBINE 20 cm
c=============================================================
c - Tpoch - temperatura pochvi na glubine 20 cm
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         if((usl2(j)>=1) .and. (usl2(j) <=3))  Tpoch(j)=ts(j)+2.7
         if((usl2(j)>=4) .and. (usl2(j) <=6)) Tpoch(j)=ts(j)+0.74
         if((usl2(j)>=7) .and. (usl2(j) <=9)) Tpoch(j)=ts(j)-0.6
         if((usl2(j)>=10) .and. (usl2(j) <=12)) Tpoch(j)=ts(j)+1.9
         if((usl2(j)>=13) .and. (usl2(j) <=15)) Tpoch(j)=ts(j)+4.4
         if((usl2(j)>=16) .and. (usl2(j) <=18)) Tpoch(j)=ts(j)+4.2
         if((usl2(j)>=19) .and. (usl2(j) <=21)) Tpoch(j)=ts(j)+3.1
         if((usl2(j)>=22) .and. (usl2(j) <=24)) Tpoch(j)=ts(j)+2.8
         if((usl2(j)>=25) .and. (usl2(j) <=27)) Tpoch(j)=ts(j)+2.1
         if((usl2(j)>=28) .and. (usl2(j) <=30)) Tpoch(j)=ts(j)+2.2
         if((usl2(j)>=31) .and. (usl2(j) <=33)) Tpoch(j)=ts(j)+4.5
         if((usl2(j)>=34) .and. (usl2(j) <=36)) Tpoch(j)=ts(j)+3.1

c=================================================================
c       RASCHET  KOEFFIZIENTOV  BAZOVOGO I  VSPOMAGATELNIX URAVNENIY
c  dlj rascheta razlogenij organicheskogo materiala pochvi
c==================================================================
c         raC(j)=47.9/(1+exp(106/(Tpoch(j)+18.3)))
         raC(j)=47.9/(1+exp(125/(Tpoch(j)+18.3)))
         if(Tpoch(j).lt.0) raC(j)=0

         rmt1(j)=47.9/(1+exp(125/(Tpoch(j)+18.3)))
         if(Tpoch(j).lt.0) rmt1(j)=0  
c  raC - koeffizient "a" v osnovnom uravnenii



c         raC(j)=47.9/(1+exp(106/(Tpoch(j)+18.3)))
c         if(Tpoch(j).lt.0)raC(j)=0  
cc  raC - koeffizient "a" v osnovnom uravnenii


c ratX - velichina "x" -otnoscenie CO2/(BIO+HUM)
c  rd41 - AccTSMD
         rbC(j)=0.2+(1.0-0.2)*((rd51(j)-rd41)/(rd51(j)-0.444*rd51(j)))
c  rbC - koeffizient "b" v osnovnom  uravnenii
c       if(rd41.lt.(0.444*rd51(n)))rbC(j)=1.0
c decompositio constsnts (k) v osnovnom uravnenii
c DPM=10.0;  RPM=0.3 ; BIO=0.66 ; HUM=0.02

c=================================================================
c  RASCHET VSPOMOGATELNIX  KOEFFIZIENTOV  DLJ  RASCHETA PROZESSA
c              NITROFIKAZII
c================================================================
c           mt2(36),mW2(36),mpH2(36) 
         rmt2(j)=-0.06+0.13*exp(0.07*Tpoch(j))

ccc        rmW2(j)=1-((1-0.2)*(inf(16)-Whgr(j)-(inf(16)-inf(15))/2))/
ccc     6 ((inf(16)-inf(15))/2)
ccc         if((inf(16)-Whgr(j)).gt.(inf(16)-inf(15))/2) rmW2(j)=1.0
ccc         if(Whgr(j).gt.inf(16)) rmW2(j)=1-((1-0.2)*(Whgr(j)-inf(16)))/
ccc     6  (inf(17)-inf(16))

         rchW2(j)=Whgr(j)/inf(17)

        rmW2(j)=(((rchW2(j)-1.27)/(0.60-1.27))**(2.84*((1.27-0.60)/(0.60-0.0012))))*((rchW2(j)-0.0012)/(0.60-0.0012))**2.84

cccc	   rmpH2(j)=0.56+3.14/(tan(3.14*0.45*(inf(18)-5)))

cccccccccccccccccccccccccccccc	 rmpH2(j)=0.56+(1/(tan(3.14*0.45*(inf(18)-5))))/3.14

	      rmpH2(j)=0.56+(atan(3.14*0.45*(inf(18)-5)))/3.14


             

c=================================================================
c  RASCHET VSPOMOGATELNIX  KOEFFIZIENTOV  DLJ  RASCHETA PROZESSA
c              DENITROFIKAZII
c================================================================
	      denNO3(j)=NNO3/((3.3*5)+NNO3)
        denW(j)= (((Whgr(j)/inf(17))-0.62)/0.38)**1.74
	      denCO2(j)=0.005*RtCO
        denpW(j)=0.5*(Whgr(j)/inf(17))
ccccccccccc        dnpNO3(j)=1-(NNO3/(VNdeni(j)*10))
	      dnpNO3(j)=0.4
c   !!! opredelit  znachenie  v znamenatele, poka stoit "10" ili d0 v formule
c po idee drob dolgna bit menee 1, sledovatelno d0 dolgno bit bolschim, t.k.
c  NNO3 budet mensche , chem to, chto vozniklo pri denitrifikazii




c==================================================================
c       RASCHET  RASTITELNIX  OSTATKOV  s.-x. kultur
c==================================================================
         Ur=inf(6)
c  inf(5) - chislo mesjzev vegetazii
         if(inf(4).eq.1)SumRst=(0.48*Ur+2.0)
         if(inf(4).eq.2)SumRst=(0.21*Ur+1.7)
         if(inf(4).eq.3)SumRst=(0.29*Ur+0.9)
         if(inf(4).eq.4)SumRst=(0.47*Ur+1.4)
         if(inf(4).eq.5)SumRst=(0.10*Ur+0.6)
         if(inf(4).eq.6)SumRst=(0.40*Ur+3.8)
         if(inf(4).eq.7)SumRst=(0.37*Ur+2.93)
         if(inf(4).eq.0)SumRst=0.001
c=================================================================
c     RASCHET  RASTITELNIX  OSTATKOV prirodnoy rastitelnosti
c=================================================================

         if(inf(10) .eq. 1)  SumRst=inf(12)*inf(11)+(inf(11)/inf(13))*inf(14)
         if(inf(10) .eq. 2)  SumRst=inf(12)*inf(11)+(inf(11)/inf(13))*inf(14)
         if(inf(10) .eq. 3)  SumRst=inf(12)*inf(11)+(inf(11)/inf(13))*inf(14)
         if(inf(10) .eq. 4)  SumRst=inf(12)*inf(11)+(inf(11)/inf(13))*inf(14)
         if(inf(10) .eq. 5)  SumRst=inf(12)*inf(11)+(inf(11)/inf(13))*inf(14)
         if(inf(10) .eq. 6)  SumRst=inf(12)*inf(11)+(inf(11)/inf(13))*inf(14)

         if(inf(10) .eq. 7) then
            SumRst=((0.311*(inf(11)/1000*inf(21))-6.067)+(0.210*(inf(11)/1000*inf(21))+47.33)*inf(14))*inf(21)  
         end if

         if(inf(10) .eq. 8) then 
            SumRst=((0.289*(inf(11)/1000*inf(21))-37.241)+(0.180*(inf(11)/1000*inf(21))+71.401)*inf(14))*inf(21)
         end if

         if(inf(10) .eq. 9) then
            SumRst=((0.097*(inf(11)/1000*inf(21))+339.250)+(0.061*(inf(11)/1000*inf(21))+662.6)*inf(14))*inf(21)
         end if

         if(inf(10) .eq. 10) then
            SumRst=((0.106*(inf(11)/1000*inf(21))-91.386)+(0.132*(inf(11)/1000*inf(21))+1885.6)*inf(14))*inf(21)  
         end if
  

c 6. Розрахунок рослинних залишків для природної рослинності:
c 1) вологі луки з домінуванням Deschampsia caespitosa ,
c якщо    ,   то    ;
c ; ; ;
c
c 2) вологі луки з домінуванням Molinia caerulea ,
c якщо    ,   то    ;
c 3) угруповання жорстких безлистих злаковидних рослин з родин
c        Cyperaceae та Juncaceae,
c якщо    ,   то    ;
c 4) угруповання низьких кореневищних та низькокупинних осок,
c якщо    ,   то    ;
c 5) угруповання високих купинних осок,
c якщо    ,   то    ;
c 6) зарості високотравних кореневищних гелофітів (очерет, рогоз),
c якщо    ,   то    ;
c 7) верба віком до 7 років,   якщо    ,   то
c  ;
c 8) верба віком 8–10 років,   якщо    ,   то
c  ;
c 9) верба віком 10–12 років,  якщо   ,  то
c  ;
c 10) верба віком більше 12 років,  якщо   ,  то
c  .



c==================================================================
c      RASCHET RASPREDELENIJ RASTITELNIX OSTATKOV PO MESJZAM VEGETAZII
c===================================================================
c          "inf(7) - nomer pervogo mesjza vegetazii kulturi"

         CUrost(j)=(((2.3026*(2./3)*10.**(2.-(2./3)*((j-inf(7)+1)/3)))/(1.+10.**(2.-(2./3)*((j-inf(7)+1)/3)))**2)/urostCoeff)*SumRst
         if(j.lt.inf(7)) CUrost(j)=0
         if(j.gt.(inf(7)+inf(5))) CUrost(j)=0

         tmpVal5 = tmpVal5 + CUrost(j)
c================================================================
c      RASCHET  PERVICHNOGO RAZLOGENIJ RASTITELNIX OSTATKOV
c================================================================
c       DPM0(j)=0.59*CUrost(j)/30
c       RPM0(j)=0.41*CUrost(j)/30

         DPM0(j)=0.59*CUrost(j)
         RPM0(j)=0.41*CUrost(j)



         DPM(j)=DPM0(j)*(1-exp(-rmt1(j)*rmW(j)*rmpH(j)*0.6*10.0*0.00278))
         RPM(j)=RPM0(j)*(1-exp(-rmt1(j)*rmW(j)*rmpH(j)*0.6*0.3*0.00278))
c================================================================
c      Razlogenie DPM na DBIO, DHUM,  DCO2
c================================================================
         DBIO(j)=DPM(j)*(1/(1+ratX(j)))*0.46
         DHUM(j)=DPM(j)*(1/(1+ratX(j)))*0.54
         DCO2(j)=DPM(j)-(DBIO(j)+DHUM(j))
        
         if(hgr(j) < -20)CHBIO(j)=0

         if(hgr(j) < -20)CHHUM(j)=0

         CHBIO(j)=DBIO(j)*rmW1(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.66*0.00278))*(1-(0.85/(1+rE(j))))
         if(hgr(j) < -20)CHBIO(j)=0


         CHHUM(j)=DHUM(j)*rmW1(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.02*0.00278))*(1-0.85*(0.85/(1+rE(j))))
         if(hgr(j) < -20)CHHUM(j)=0



c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c=================================================================
c      Razlogenie RPM na BIO1,  HUM1, R1CO2
c================================================================
         BIO1(j)=RPM(j)*(1/(1+ratX(j)))*0.46
         HUM1(j)=RPM(j)*(1/(1+ratX(j)))*0.54

c       BIO1(j)=RPM(j)*(1-((1/ratX(j))-(0.85/(1+(1/ratX(j)))))
c     5-(0.85/(1
c     5+(1/ratX(j)))))*0.46
c       HUM1(j)=RPM(j)*(0.85/(1+(1/ratX(j))))*0.54
         R1CO2(j)=RPM(j)-(BIO1(j)+HUM1(j))

c       R1CO2(j)=RPM(j)*(1-((1/ratX(j))))


         CHBIO1(j)=BIO1(j)*rmW1(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.66*0.00278))*(1-(0.85/(1+rE(j))))
         if(hgr(j) < -20)CHBIO1(j)=0

         CHHUM1(j)=HUM1(j)*rmW1(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.02*0.00278))*(1-0.85*(0.85/(1+rE(j))))
         if(hgr(j) < -20)CHHUM1(j)=0

c      CHBIO1(j)=(1-((1/ratX(j))-(0.85/(1+(1/ratX(j)))))-(0.85/(1
c     5+(1/ratX(j)))))
c     6 *(BIO1(j)*rmW3(j)*(exp(-rmt1(j)
c     6 *rmpH1(j)*0.6*0.66*0.08333)))
         if(hgr(j) < -20)CHBIO1(j)=0

c      CHHUM1(j)=(1-((1/ratX(j))-(0.85/(1+(1/ratX(j)))))-(0.85/(1
c     5+(1/ratX(j)))))
c     6 *(HUM1(j)*rmW3(j)*(exp(-rmt1(j)*rmpH1(j)
c     6 *0.6*0.02*0.08333)))
         if(hgr(j) < -20)CHHUM1(j)=0




c================================================================
c      Razlogenie BIO1 na  BIO2,  HUM2, R2CO22
c===============================================================
c       rdh1(j)=(DPM(j)*(exp(-rmt1(j)*rmW1(j)*rmpH1(j)*0.6*0.66*0.00278)
c     6  )
c     6+DBIO(j))*(1-exp(-rmt1(j)*rmW1(j)*rmpH1(j)*0.6*0.66*0.00278))

         rdh1(j)=BIO1(j)*(1-exp(-rmt1(j)*rmW(j)*rmpH(j)*0.6*0.66*0.00278))

         BIO2(j)=rdh1(j)*(1/(1+ratX(j)))*0.46

c       rdh2(j)=(DPM(j)*(exp(-rmt1(j)*rmW1(j)*rmpH1(j)*0.6*0.02*0.08333)
c     6  )
c     6+DHUM(j))*(1-exp(-rmt1(j)*rmW1(j)*rmpH1(j)*0.6*0.02*0.08333))

cc       rdh2(j)=DHUM(j))*(1-exp(-rmt1(j)*rmW1(j)*rmpH1(j)*0.6*0.02*0.08333))

         rdh2(j)=BIO1(j)*(1-exp(-rmt1(j)*rmW(j)*rmpH(j)*0.6*0.02*0.00278))


     
         HUM2(j)=rdh2(j)*(1/(1+ratX(j)))*0.54

         R2CO2(j)=BIO1(j)-(BIO2(j)+HUM2(j))


c      CHBIO2(j)=(1-((1/ratX(j))-(0.85/(1+(1/ratX(j)))))-(0.85/(1
c     5+(1/ratX(j)))))
c     6 *(BIO2(j)*(exp(-rmt1(j)*rmW1(j)
c     6 *rmpH1(j)*0.6*0.66*0.08333)))
         if(hgr(j) < -20)CHBIO2(j)=0

c      CHHUM2(j)=(1-((1/ratX(j))-(0.85/(1+(1/ratX(j)))))-(0.85/(1
c     5+(1/ratX(j)))))
c     6 *(HUM2(j)*(exp(-rmt1(j)*rmW1(j)*rmpH1(j)
c     6 *0.6*0.02*0.08333)))
         if(hgr(j) < -20)CHHUM2(j)=0

         CHBIO2(j)=BIO2(j)*rmW1(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.66*0.00278))*(1-(0.85/(1+rE(j))))
         if(hgr(j) < -20)CHBIO2(j)=0

         CHHUM2(j)=HUM2(j)*rmW3(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.02*0.00278))*(1-0.85*(0.85/(1+rE(j))))
         if(hgr(j) < -20)CHHUM2(j)=0



c===================================================================
c      Razlogenie  HUM1 na BIO3,  HUM3, R3CO2
c=================================================================
c	rdh3(j)=(RPM(j)*(exp(-raC(j)*rmW(j)*rmpH(j)*0.6*0.66*0.08333)
c     6   )+
c     6 DBIO(j)+BIO2(j))*(1-exp(-raC(j)*rmW(j)*rmpH(j)*0.6*0.66*0.08333)*
c     6 usl4(j))

         rdh3(j)=HUM1(j)*(1-exp(-rmt1(j)*rmW(j)*rmpH(j)*0.6*0.66*0.00278))



     
         BIO3(j)=rdh3(j)*(1/(1+ratX(j)))*0.46
	

c	rdh4(j)=(RPM(j)*(1-exp(-raC(j)*rmW(j)*rmpH(j)*0.6*0.02*0.08333)
c     6   )
c     6+DHUM(j)+HUM2(j))*(1-exp(-raC(j)*rmW(j)*rmpH(j)*0.6*0.02*0.08333)*
c     6 usl4(j))

	rdh4(j)=HUM1(j)*(1-exp(-rmt1(j)*rmW(j)*rmpH(j)*0.6*0.02*0.00278))

        HUM3(j)=rdh4(j)*(1/(1+ratX(j)))*0.54		

        R3CO(j)=HUM1(j)-(BIO3(j)+HUM3(j))

c      CHBIO3(j)=(1-((1/ratX(j))-(0.85/(1+(1/ratX(j)))))-(0.85/(1
c     5+(1/ratX(j)))))
c     6 *(BIO3(j)*(exp(-raC(j)*rmW(j)
c     6 *rmpH(j)*0.6*0.66*0.08333)))
         if(hgr(j) < -20)CHBIO3(j)=0

c      CHHUM3(j)=(1-((1/ratX(j))-(0.85/(1+(1/ratX(j)))))-(0.85/(1
c     5+(1/ratX(j)))))
c     6 *(HUM3(j)*(exp(-raC(j)*rmW(j)*rmpH(j)
c     6 *0.6*0.02*0.08333)))
         if(hgr(j) < -20)CHHUM3(j)=0
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         CHBIO3(j)=BIO3(j)*(exp(-raC(j)*rmW(j)*rmpH(j)*0.6*0.66*0.00278))*(1-(0.85/(1+rE(j))))
         if(hgr(j) < -20)CHBIO3(j)=0

         CHHUM3(j)=HUM3(j)*(exp(-raC(j)*rmW(j)*rmpH(j)*rmpH(j)*0.6*0.02*0.00278))*(1-0.85*(0.85/(1+rE(j))))
         if(hgr(j) < -20)CHHUM3(j)=0

c================================================================
c      RASCHET  PERVICHNOGO RAZLOGENIJ NERAZLOGIVSCHIXSJ RASTITELNIX OSTATKOV
c            PREDSCHESTVJUSCHEGO  GODA
c================================================================


         PrDPM0(j)=0.59*inf(1)
         PrRPM0(j)=0.41*inf(1)
c       PrDPM(j)=PrDPM0(j)*((1-exp(-rmt1(j)*rmW1(j)*rmpH1(j)*0.6*10.0
c     6*0.00278)))*0.05
c       PrRPM(j)=PrRPM0(j)*((1-exp(-rmt1(j)*rmW1(j)*rmpH1(j)*0.6*0.3
c     6*0.00278)))*0.05

c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         PrDPM(j)=PrDPM0(j)*((1-exp(-rmt1(j)*rmW(j)*rmpH(j)*0.6*10.0*0.00278)))*0.3
         PrRPM(j)=PrRPM0(j)*((1-exp(-rmt1(j)*rmW(j)*rmpH(j)*0.6*0.3*0.00278)))*0.05


c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c================================================================
c      Razlogenie PrDPM na PrDBIO, PrDHUM,  PrDCO2
c================================================================

c          PrDBIO(j)=PrDPM(j)*(0.85)*(0.85/(1+(1/ratX(j))))*0.46
c          PrDHUM(j)=PrDPM(j)*(0.85/(1+(1/ratX(j))))*0.54

         PrDBIO(j)=PrDPM(j)*(1/(1+ratX(j)))*0.46
         PrDHUM(j)=PrDPM(j)*(1/(1+ratX(j)))*0.54



         PrDCO2(j)=PrDPM(j)-(PrDBIO(j)+PrDHUM(j))

c      PCHBIO(j)=(1-((1/ratX(j))-(0.85/(1+(1/ratX(j)))))-(0.85/(1
c     5+(1/ratX(j)))))
c     6 *(PrDBIO(j)*(exp(-rmt1(j)*rmW1(j)
c     6 *rmpH1(j)*0.6*0.66*0.08333)))
         if(hgr(j) < -20)PCHBIO(j)=0

c      PCHHUM(j)=(1-((1/ratX(j))-(0.85/(1+(1/ratX(j)))))-(0.85/(1
c     5+(1/ratX(j)))))
c     6 *(PrDHUM(j)*(exp(-rmt1(j)*rmW1(j)*rmpH1(j)
c     6 *0.6*0.02*0.08333)))
         if(hgr(j) < -20)PCHHUM(j)=0

         PCHBIO(j)=PrDBIO(j)*rmW1(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.66*0.00278))*(1-(0.85/(1+rE(j))))

         if(hgr(j) < -20)PCHBIO(j)=0

         PCHHUM(j)=PrDHUM(j)*rmW1(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.02*0.00278))*(1-0.85*(0.85/(1+rE(j))))

         if(hgr(j) < -20)PCHHUM(j)=0

c=================================================================
c      Razlogenie PrRPM na PrBIO1,  PrHUM1, PrR1CO2
c================================================================
c       PrBIO1(j)=PrRPM(j)*(0.85)*(0.85/(1+(1/ratX(j))))*0.45
c       PrHUM1(j)=PrRPM(j)*(0.85/(1+(1/ratX(j))))*0.54
         PrBIO1(j)=PrRPM(j)*(1/(1+ratX(j)))*0.45
         PrHUM1(j)=PrRPM(j)*(1/(1+ratX(j)))*0.54



         PrR1CO(j)=PrRPM(j)-(PrBIO1(j)+PrHUM1(j))

c       PCHB1(j)=(1-((1/ratX(j))-(0.85/(1+(1/ratX(j)))))-(0.85/(1
c     5+(1/ratX(j)))))
c     6 *(PrBIO1(j)*(exp(-rmt1(j)*rmW1(j)
c     6 *rmpH1(j)*0.6*0.66*0.08333)))
         if(hgr(j) < -20)PCHB1(j)=0

c       PCHH1(j)=(1-((1/ratX(j))-(0.85/(1+(1/ratX(j)))))-(0.85/(1
c     5+(1/ratX(j)))))
c     6 *(PrHUM1(j)*(exp(-rmt1(j)*rmW1(j)*rmpH1(j)
c     6 *0.6*0.02*0.08333)))
         if(hgr(j) < -20)PCHH1(j)=0

         PCHB1(j)=PrBIO1(j)*rmW1(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.66*0.00278))*(1-(0.85/(1+rE(j))))
         if(hgr(j) < -20) PCHB1(j)=0

         PCHH1(j)=PrHUM1(j)*rmW1(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.02*0.00278))*(1-0.85*(0.85/(1+rE(j))))

         if(hgr(j) < -20) PCHH1(j)=0

c================================================================
c      Razlogenie PrDBIO na  PrBIO2,  PrHUM2, PrR2CO22
c===============================================================
c      Prrdh1(j)- kolichestvo  BIO
c       Prrdh1(j)=(PrDPM(j)*(exp(-raC(j)*rbC(j)*0.6*0.66*0.08333))
c     6 +PrDBIO(j))*(1-exp(-raC(j)*rbC(j)*0.6*0.66*0.08333))
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         Prrdh1(j)=PrDBIO(j)*(1-exp(-rmt1(j)*rmW(j)*rmpH(j)*0.6*0.66*0.00278))

         PrBIO2(j)=Prrdh1(j)*(1/(1+ratX(j)))*0.46

c       Prrdh2(j)=(PrDPM(j)*(exp(-raC(j)*rbC(j)*0.6*0.02*0.08333))
c     6 +PrDHUM(j))*(1-exp(-raC(j)*rbC(j)*0.6*0.02*0.08333))

         Prrdh2(j)=PrDBIO(j)*(1-exp(-rmt1(j)*rmW(j)*rmpH(j)*0.6*0.02*0.00278))


         PrHUM2(j)=Prrdh2(j)*(1/(1+ratX(j)))*0.54

ccccccc       PrR2CO(j)=(rdh1(j)*0.46+Prrdh2(j)*0.54)-(PrBIO2(j)+PrHUM2(j))

         PrR2CO(j)=PrDBIO(j)-(PrBIO2(j)+PrHUM2(j))


c       PCHB2(j)=(1-((1/ratX(j))-(0.85/(1+(1/ratX(j)))))-(0.85/(1
c     5+(1/ratX(j)))))
c     6 *(PrBIO2(j)*(exp(-rmt1(j)*rmW1(j)
c     6 *rmpH1(j)*0.6*0.66*0.08333)))
         if(hgr(j) < -20)PCHB2(j)=0

c       PCHH2(j)=(1-((1/ratX(j))-(0.85/(1+(1/ratX(j)))))-(0.85/(1
c     5+(1/ratX(j)))))
c     6 *(PrHUM2(j)*(exp(-rmt1(j)*rmW1(j)*rmpH1(j)
c     6 *0.6*0.02*0.08333)))
         if(hgr(j) < -20)PCHH2(j)=0

         PCHB2(j)=PrBIO2(j)*rmW1(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.66*0.00278))*(1-(0.85/(1+rE(j))))
         if(hgr(j) < -20)PCHB2(j)=0

         PCHH2(j)=PrHUM2(j)*rmW1(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.02*0.00278))*(1-0.85*(0.85/(1+rE(j))))
         if(hgr(j) < -20)PCHH2(j)=0

c===================================================================
c      Razlogenie  PrHUM1 na PrBIO3,  PrHUM3, PrR3CO2
c=================================================================
	      Prrdh3(j)=PrHUM1(j)*(1-exp(-rmt1(j)*rmW(j)*rmpH(j)*0.6*0.66*0.00278))

c        PrBIO3(j)=Prrdh3(j)*(0.85)*(0.85/(1+(1/ratX(j))))*0.46
         PrBIO3(j)=Prrdh3(j)*(1/(1+ratX(j)))*0.46

	      Prrdh4(j)=PrHUM1(j)*(1-exp(-rmt1(j)*rmW(j)*rmpH(j)*0.6*0.02*0.00278))

c       PrHUM3(j)=Prrdh4(j)*(0.85/(1+(1/ratX(j))))*0.54
         PrHUM3(j)=Prrdh4(j)*(1/(1+ratX(j)))*0.54

         PrR3CO(j)=PrHUM1(j)-(PrBIO3(j)+PrHUM3(j))


c       PCHB3(j)=(1-((1/ratX(j))-(0.85/(1+(1/ratX(j)))))-(0.85/(1
c     5+(1/ratX(j)))))
c     6 *(PrBIO3(j)*rmW3(j)*(exp(-rmt1(j)
c     6 *rmpH1(j)*0.6*0.66*0.08333)))
         if(hgr(j) < -20)PCHB3(j)=0

c       PCHH3(j)=(1-((1/ratX(j))-(0.85/(1+(1/ratX(j)))))-(0.85/(1
c     5+(1/ratX(j)))))
c     6 *(PrHUM3(j)*rmW3(j)*(exp(-rmt1(j)*rmpH1(j)
c     6 *0.6*0.02*0.08333)))
         if(hgr(j) < -20)PCHH3(j)=0

         PCHB3(j)=PrBIO3(j)*rmW1(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.66*0.00278))*(1-(0.85/(1+rE(j))))
         if(hgr(j) < -20)PCHB3(j)=0

         PCHH3(j)=PrHUM3(j)*rmW1(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.02*0.00278))*(1-0.85*(0.85/(1+rE(j))))

         if(hgr(j) < -20)PCHH3(j)=0
c====================================================================
c    Summarnoe razlogenie po vsem komponentam  rastitelnix ostatkov
c     i skorosti mineralizazii
c====================================================================
	      BIOrst(j)=(DBIO(j)+PrDBIO(j)+BIO1(j)+PrBIO1(j)+BIO2(j)+PrBIO2(j)+BIO3(j)+PrBIO3(j))

	      HUMrst(j)=(DHUM(j)+PrDHUM(j)+HUM1(j)+PrHUM1(j)+HUM2(j)+PrHUM2(j)+HUM3(j)+PrHUM3(j))
	
      	SMCrst(j)=(BIOrst(j)+HUMrst(j))*10

cccc        SMNrst(j)=SMCrst(j)/rnitr(3)*1000*30
         SMNrst(j)=SMCrst(j)*1000/rnitr(3)
cccccccccccccccc        if(j.gt.1) SMNrst(j)=SMCrst(j)/CNrst(j)  

c	CO2rst(j)=(DCO2(j)+PrDCO2(j)+R1CO2(j)+PrR1CO(j)+R2CO2(j)
c     6   +PrR2CO(j)+R3CO(j)+PrR3CO(j))*30

ccccccccccccc	CO2rst(j)=(DCO2(j)+R1CO2(j)+R2CO2(j)
cccccccccccccccccc     6   +R3CO(j))*30

	      CO2rst(j)=(DCO2(j)+R1CO2(j)+R2CO2(j)+R3CO(j))*10


	      SMCOrs=SMCOrs+CO2rst(j)	

	      prCOrs(j)=(PrDCO2(j)+PrR1CO(j)+R2CO2(j)+PrR2CO(j)+PrR3CO(j))*10
	      
         prSMCO=prSMCO+prCOrs(j)

c       CHBIrs(j)=CHBIO(j)*usl3(j)*30+CHBIO1(j)*usl3(j)*30+CHBIO2(j)
c     6  *usl3(j)*30
c     6 +CHBIO3(j)*usl3(j)*30+PCHBIO(j)*usl3(j)*30+
c     6 PCHB1(j)*usl3(j)*30+PCHB2(j)*usl3(j)*30+PCHB3(j)*usl3(j)*30
cc
c	CHHUrs(j)=CHHUM(j)*usl3(j)*30+CHHUM1(j)*usl3(j)*30+CHHUM2(j)*
c     6   usl3(j)*30
c     6 +CHHUM3(j)*usl3(j)*30+PCHHUM(j)*usl3(j)*30+
c     6 PCHH1(j)*usl3(j)*30+PCHH2(j)*usl3(j)*30+PCHH3(j)*usl3(j)*30
cc
c
	      CHBIrs(j)=(CHBIO(j)+CHBIO1(j)+CHBIO2(j)+CHBIO3(j)+PCHBIO(j)+PCHB1(j)+PCHB2(j)+PCHB3(j))*10 
         
     

	      CHHUrs(j)=(CHHUM(j)+CHHUM1(j)+CHHUM2(j)+CHHUM3(j)+PCHHUM(j)+PCHH1(j)+PCHH2(j)+PCHH3(j))*10
	      

     
         CHrst(j)=CHBIrs(j)+CHHUrs(j)
c	If(usl3(j).eq.0) CHrst(j)=0
c	If(usl3(j).eq.1) CHrst(j)=(CHBIrs(j)+CHHUrs(j))*0.33
c	If(usl3(j).eq.1) CHrst(j)=(CHBIrs(j)+CHHUrs(j))*0.66
c	If(usl3(j).eq.1) CHrst(j)=(CHBIrs(j)+CHHUrs(j))*1.0
    
	      SMCHrs=SMCHrs+CHrst(j)

c===================================================================
c  RASCHET SODERGJANIJ INERTNOGO ORGANICHNOGO MATERIALA
c==================================================================
         RIOM(j)=0.049*(inf(8)**1.139)

c===================================================================
c RASCHET NACHALNIX DANNIX KOMPONENTOV ORGANICHNOGO MATERIALA
c                          POCHVI
c================================================================
c	rNmicr(j)=(VNnitr(j)*1000000000)/(inf(19)*0.2*10000*1000000)
c	rNmicr(j)=VNnitr(j)/(inf(19)*0.2*10)
         rMCsoi(j)=(inf(19)*inf(8)*10)*(inf(20)/100) 



         SDPM(j)=rnitr(12)*rMCsoi(j)
         SRPM(j)=rnitr(13)*rMCsoi(j)
         SBIO(j)=rnitr(14)*rMCsoi(j)
         SHUM(j)=rnitr(15)*rMCsoi(j)
c       write(OutputFileUnit, *) "rnitr(12) -OMSDPM(j) 
c       write(OutputFileUnit, *) "rnitr(13) -OMSRPM(j) 
c       write(OutputFileUnit, *) "rnitr(14) -OMSBIO(j) 
c       write(OutputFileUnit, *) "rnitr(15) -OMSHUM(j) 


c=============================================================
c    RASCHET  ORGANICHNOGO MATERIALA  POCHVI
c=============================================================

c        SDPM0(j)=SDPM(j)*(1-exp(-raC(j)*rbC(j)*0.6*10.0*0.08333))
c        SRPM0(j)=SRPM(j)*(1-exp(-raC(j)*rbC(j)*0.6*0.3*0.08333))
c        SBIO0(j)=SBIO(j)*(1-exp(-raC(j)*rbC(j)*0.6*0.66*0.08333))
c        SHUM0(j)=SHUM(j)*(1-exp(-raC(j)*rbC(j)*0.6*0.02*0.08333))



         SDPM0(j)=SDPM(j)*(1-exp(-rmt1(j)*rmW(j)*rmpH(j)*0.6*10.0*0.00278))
         SRPM0(j)=SRPM(j)*(1-exp(-rmt1(j)*rmW(j)*rmpH(j)*0.6*0.3*0.00278))
         SBIO0(j)=SBIO(j)*(1-exp(-rmt1(j)*rmW(j)*rmpH(j)*0.6*0.66*0.00278))
         SHUM0(j)=SHUM(j)*(1-exp(-rmt1(j)*rmW(j)*rmpH(j)*0.6*0.02*0.00278))



c       HSBIO0(j)=(1-SBIO0(j)-SHUM0(j))*(SBIO0(j)*(exp(-raC(j)*rmW(j)
c     6 *rmpH(j)*0.6*0.66*0.08333)))
c       HHUM0(j)=(1-SBIO0(j)-SHUM0(j))*(SHUM0(j)*(exp(-raC(j)*rmW(j)
c     6 *rmpH(j)*0.6*0.02*0.08333)))

c      HSBIO0(j)=(1-((1/ratX(j))-(0.85/(1+(1/ratX(j)))))-(0.85/(1
c     5+(1/ratX(j)))))
c     6 *(SBIO0(j)*(exp(-rmt1(j)*rmW1(j)
c     6 *rmpH1(j)*0.6*0.66*0.08333)))
         if(hgr(j) < -20) HSBIO0(j)=0

c      HHUM00(j)=(1-((1/ratX(j))-(0.85/(1+(1/ratX(j)))))-(0.85/(1
c     5+(1/ratX(j)))))
c     6 *(SHUM0(j)*(exp(-rmt1(j)*rmW1(j)*rmpH1(j)
c     6 *0.6*0.02*0.08333)))
         if(hgr(j) < -20) HHUM00(j)=0


         HSBIO0(j)=SBIO0(j)*rmW1(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.66*0.00278))*(1-(0.85/(1+rE(j))))
         if(hgr(j) < -20) HSBIO0(j)=0

         HHUM00(j)=SHUM0(j)*rmW1(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.02*0.00278))*(1-0.85*(0.85/(1+rE(j))))

         if(hgr(j) < -20) HHUM00(j)=0



c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c============================================================
c RASCHET  RAZLOGENIJ  SDPM0(j)  POCHVI
c=============================================================
c	SDBIO(j)=SDPM0(j)*(0.85)*(0.85/(1+(1/ratX(j))))*0.46
c        SDHUM(j)=SDPM0(j)*(0.85/(1+(1/ratX(j))))*0.54

	       SDBIO(j)=SDPM0(j)*(1/(1+ratX(j)))*0.46
         SDHUM(j)=SDPM0(j)*(1/(1+ratX(j)))*0.54


         SDCO(j)=SDPM0(j)-(SDBIO(j)+SDHUM(j))


c       HSBIO(j)=(1-SDBIO(j)-SDHUM(j))*(SDBIO(j)*(exp(-raC(j)*rmW(j)
c     6 *rmpH(j)*0.6*0.66*0.08333)))
c       HHUM0(j)=(1-SBIO0(j)-SDHUM(j))*(SDHUM(j)*(exp(-raC(j)*rmW(j)
c     6  *rmpH(j)*0.6*0.02*0.08333)))

c      HSBIO(j)=(1-((1/ratX(j))-(0.85/(1+(1/ratX(j)))))-(0.85/(1
c     5+(1/ratX(j)))))
c     6 *(SDBIO(j)*(exp(-rmt1(j)*rmW1(j)
c     6 *rmpH1(j)*0.6*0.66*0.08333)))
         if(hgr(j) < -20) HSBIO(j)=0

c       HHUM0(j)=(1-((1/ratX(j))-(0.85/(1+(1/ratX(j)))))-(0.85/(1
c     5+(1/ratX(j)))))
c     6 *(SDHUM(j)*(exp(-rmt1(j)*rmW1(j)*rmpH1(j)
c     6 *0.6*0.02*0.08333)))
         if(hgr(j) < -20) HHUM0(j)=0

         HSBIO(j)=SDBIO(j)*rmW1(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.66*0.00278))*(1-(0.85/(1+rE(j))))
         if(hgr(j) < -20) HSBIO(j)=0

         HHUM0(j)=SDHUM(j)*rmW1(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.02*0.00278))*(1-0.85*(0.85/(1+rE(j))))

         if(hgr(j) < -20) HHUM0(j)=0




c=============================================================
c              RASCHET  RAZLOGENIJ  SRPM0(j)  POCHVI
c=============================================================
c	SBIO1(j)=SRPM0(j)*(0.85)*(0.85/(1+(1/ratX(j))))*0.46
c        SHUM1(j)=SRPM0(j)*(0.85/(1+(1/ratX(j))))*0.54

	      SBIO1(j)=SRPM0(j)*(1/(1+ratX(j)))*0.46
         SHUM1(j)=SRPM0(j)*(1/(1+ratX(j)))*0.54



         SR1CO(j)=SRPM0(j)-(SBIO1(j)+SHUM1(j))

c       HSBIO1(j)=(1-SBIO1(j)-SHUM1(j))*(SBIO1(j)*(exp(-raC(j)*rmW(j)
c     6 *rmpH(j)*0.6*0.66*0.08333)))
c       HHUM1(j)=(1-SBIO1(j)-SHUM1(j))*(SHUM1(j)*(exp(-raC(j)*rmW(j)
c     6 *rmpH(j)*0.6*0.02*0.08333)))

c      HSBIO1(j)=(1-((1/ratX(j))-(0.85/(1+(1/ratX(j)))))-(0.85/(1
c     5+(1/ratX(j)))))
c     6 *(SBIO1(j)*(exp(-rmt1(j)*rmW1(j)
c     6 *rmpH1(j)*0.6*0.66*0.08333)))
         if(hgr(j) < -20)HSBIO1(j)=0

c       HHUM1(j)=(1-((1/ratX(j))-(0.85/(1+(1/ratX(j)))))-(0.85/(1
c     5+(1/ratX(j)))))
c     6 *(SHUM1(j)*(exp(-rmt1(j)*rmW1(j)*rmpH1(j)
c     6 *0.6*0.02*0.08333)))
         if(hgr(j) < -20)HHUM1(j)=0

         HSBIO1(j)=SBIO1(j)*rmW1(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.66*0.00278))*(1-(0.85/(1+rE(j))))
         if(hgr(j) < -20)HSBIO1(j)=0

         HHUM1(j)=SHUM1(j)*rmW1(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.02*0.00278))*(1-0.85*(0.85/(1+rE(j))))

         if(hgr(j) < -20)HHUM1(j)=0



c=============================================================
c              RASCHET  RAZLOGENIJ  SBIO0(j)  POCHVI
c=============================================================
c	SBIO2(j)=SBIO0(j)*(0.85)*(0.85/(1+(1/ratX(j))))*0.46
c        SHUM2(j)=SBIO0(j)*(0.85/(1+(1/ratX(j))))*0.54

	      SBIO2(j)=SBIO0(j)*(1/(1+ratX(j)))*0.46
         SHUM2(j)=SBIO0(j)*(1/(1+ratX(j)))*0.54



         SR2CO(j)=SBIO0(j)-(SBIO2(j)+SHUM2(j))

c       HSBIO2(j)=(1-SBIO2(j)-SHUM2(j))*(SBIO2(j)*(exp(-raC(j)*rmW(j)
c     6 *rmpH(j)*0.6*0.66*0.08333)))
c       HHUM2(j)=(1-SBIO2(j)-SHUM2(j))*(SHUM2(j)*(exp(-raC(j)*rmW(j)
c     6  *rmpH(j)*0.6*0.02*0.08333)))

c       HSBIO2(j)=SBIO2(j)*(exp(-raC(j)*rmW(j)
c     6 *rmpH(j)*0.6*0.66*0.08333)))
c       HHUM2(j)=(1-SBIO2(j)-SHUM2(j))*(SHUM2(j)*(exp(-raC(j)*rmW(j)
c     6  *rmpH(j)*0.6*0.02*0.08333)))


c      HSBIO2(j)=(1-((1/ratX(j))-(0.85/(1+(1/ratX(j)))))-(0.85/(1
c     5+(1/ratX(j)))))
c     6 *(SBIO2(j)*(exp(-rmt1(j)*rmW1(j)
c     6 *rmpH1(j)*0.6*0.66*0.08333)))
         if(hgr(j) < -20)HSBIO2(j)=0

c       HHUM2(j)=(1-((1/ratX(j))-(0.85/(1+(1/ratX(j)))))-(0.85/(1
c     5+(1/ratX(j)))))
c     6 *(SHUM2(j)*(exp(-rmt1(j)*rmW1(j)*rmpH1(j)
c     6 *0.6*0.02*0.08333)))
         if(hgr(j) < -20)HHUM2(j)=0

         HSBIO2(j)=SBIO2(j)*rmW1(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.66*0.00278))*(1-(0.85/(1+rE(j))))
         if(hgr(j) < -20)HSBIO2(j)=0

         HHUM2(j)=SHUM2(j)*rmW1(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.02*0.00278))*(1-0.85*(0.85/(1+rE(j))))

         if(hgr(j) < -20)HHUM2(j)=0



c=============================================================
c              RASCHET  RAZLOGENIJ  SHUM0(j)  POCHVI
c=============================================================
c	SBIO3(j)=SHUM0(j)*(0.85)*(0.85/(1+(1/ratX(j))))*0.46
c        SHUM3(j)=SHUM0(j)*(0.85/(1+(1/ratX(j))))*0.54

	      SBIO3(j)=SHUM0(j)*(1/(1+ratX(j)))*0.46
         SHUM3(j)=SHUM0(j)*(1/(1+ratX(j)))*0.54




         SR3CO(j)=SHUM0(j)-(SBIO3(j)+SHUM3(j))

c       HSBIO3(j)=(1-SBIO3(j)-SHUM3(j))*(SBIO3(j)*(exp(-raC(j)*rmW(j)
c     6 *rmpH(j)*0.6*0.66*0.08333)))
c       HHUM3(j)=(1-SBIO3(j)-SHUM3(j))*(SHUM3(j)*(exp(-raC(j)*rmW(j)
c     6 *rmpH(j)*0.6*0.02*0.08333)))

c      HSBIO3(j)=(1-((1/ratX(j))-(0.85/(1+(1/ratX(j)))))-(0.85/(1
c     5+(1/ratX(j)))))
c     6 *(SBIO3(j)*(exp(-rmt1(j)*rmW1(j)
c     6 *rmpH1(j)*0.6*0.66*0.08333)))
         if(hgr(j) < -20)HSBIO3(j)=0

c       HHUM3(j)=(1-((1/ratX(j))-(0.85/(1+(1/ratX(j)))))-(0.85/(1
c     5+(1/ratX(j)))))
c     6 *(SHUM3(j)*(exp(-rmt1(j)*rmW1(j)*rmpH1(j)
c     6 *0.6*0.02*0.08333)))
         if(hgr(j) < -20)HHUM3(j)=0

         HSBIO3(j)=SBIO3(j)*rmW1(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.66*0.00278))*(1-(0.85/(1+rE(j))))
         if(hgr(j) < -20)HSBIO3(j)=0

         HHUM3(j)=SHUM3(j)*rmW1(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.02*0.00278))*(1-0.85*(0.85/(1+rE(j))))

         if(hgr(j) < -20)HHUM3(j)=0


c===============================================================
c    Summarnoe razlogenie po vsem komponentam pochvi
c===============================================================
         BIOsoi(j)=(SDBIO(j)+SBIO1(j)+SBIO2(j)+SBIO3(j))

         HUMsoi(j)=(SDHUM(j)+SHUM1(j)+SHUM2(j)+SHUM3(j))

	      SMCsoi(j)=(BIOsoi(j)+HUMsoi(j))*10

         SMNsoi(j)=SMCsoi(j)*1000/(rnitr(4))
c perevod v kg N/ga umnogjenie na 1000
ccccccccc        if(j.gt.1) SMNsoi(j)=SMCsoi(j)/CNsoil(j)  


          CO2soi(j)=(SDCO(j)+sR1CO(j)+sR2CO(j)+sR3CO(j))*10

         

         SMCOso=SMCOso+CO2soi(j) 




c	CHBIso(j)=HSBIO0(j)*usl3(j)*30+HSBIO(j)*usl3(j)*30+HSBIO1(j)
c     4 *usl3(j)*30+HSBIO2(j)*usl3(j)*30+HSBIO3(j)*usl3(j)*30
c
c        CHHUso(j)=HHUM00(j)*usl3(j)*30+HHUM0(j)*usl3(j)*30+HHUM1(j)*
c     4 usl3(j)*30+HHUM2(j)*usl3(j)*30+HHUM3(j)*usl3(j)*30

         CHBIso(j)=(HSBIO0(j)+HSBIO(j)+HSBIO1(j)+HSBIO2(j)+HSBIO3(j))*10

         CHHUso(j)=(HHUM00(j)+HHUM0(j)+HHUM1(j)+HHUM2(j)+HHUM3(j))*10

	      CHsoil(j)=CHBIso(j)+CHHUso(j)
c        If(usl3(j).eq.0)CHsoil(j)=0     
c        If(usl3(j).eq.1)CHsoil(j)=(CHBIso(j)+CHHUso(j))*0.33
c        If(usl3(j).eq.2)CHsoil(j)=(CHBIso(j)+CHHUso(j))*0.66
c        If(usl3(j).eq.3)CHsoil(j)=(CHBIso(j)+CHHUso(j))*1.00



	      SMCHso=SMCHso+CHsoil(j)


c==============================================================
c c  RASCHET  RAZLOGJENIJ VESCHESTVA ORGANICHESKIX UDOBRENIY
c=============================================================
c================================================================
c      RASCHET  PERVICHNOGO RAZLOGENIJ ORGANICHESKIX UDOBRENIY
c================================================================
c     0.25 -otnositelnoe sodergjanie suxogo veschestva v
c                organicheskix udobrenijx (po Obuxovu, str.72)
c    0.5 - otnositelnoe sodergjanie ugleroda v suom veschestve
         FDPM0(j)=0.49*inf(9)*0.25*0.5
         FRPM0(j)=0.49*inf(9)*0.25*0.5
         FHUM0(j)=0.02*inf(9)*0.25*0.5

         FDPM(j)=FDPM0(j)*(1-exp(-rmt1(j)*rmW(j)*rmpH(j)*0.6*10.0*0.00278))
         FRPM(j)=FRPM0(j)*(1-exp(-rmt1(j)*rmW(j)*rmpH(j)*0.6*0.3*0.00278))
         FHUM(j)=FDPM(j)*(1-exp(-rmt1(j)*rmW(j)*rmpH(j)*0.6*0.02*0.00278))


c============================================================
c     RASCHET  RAZLOGJENIJ  FDPM  UDOBRENIY
c============================================================
	      FDBIO(j)=FDPM(j)*(1/(1+ratX(j)))*0.46
         FDHUM(j)=FDPM(j)*(1/(1+ratX(j)))*0.54

         FDCO(j)=FDPM(j)-(FDBIO(j)+FDHUM(j))

         CHFBI0(j)=FDBIO(j)*rmW1(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.66*0.00278))*(1-(0.85/(1+rE(j))))
         if(hgr(j) < -20)CHFBI0(j)=0

         CHFHU0(j)=FDHUM(j)*rmW1(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.02*0.00278))*(1-0.85*(0.85/(1+rE(j))))

         if(hgr(j) < -20)CHFHU0(j)=0

c============================================================
c     RASCHET  RAZLOGJENIJ  FRPM  UDOBRENIY
c============================================================
       	FBIO1(j)=FRPM(j)*(1/(1+ratX(j)))*0.46
         FHUM1(j)=FRPM(j)*(1/(1+ratX(j)))*0.54

         FR1CO(j)=FRPM(j)-(FBIO1(j)+FHUM1(j))

         CHFBI1(j)=FBIO1(j)*rmW1(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.66*0.00278))*(1-(0.85/(1+rE(j))))
         if(hgr(j) < -20)CHFBI1(j)=0

         CHFHU1(j)=FHUM1(j)*rmW1(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.02*0.00278))*(1-0.85*(0.85/(1+rE(j))))

         if(hgr(j) < -20)CHFHU1(j)=0
c============================================================
c     RASCHET  RAZLOGJENIJ  FHUM  UDOBRENIY
c============================================================
	      FBIO3(j)=FHUM(j)*(1/(1+ratX(j)))*0.46
         FHUM3(j)=FHUM(j)*(1/(1+ratX(j)))*0.54

         FR3CO(j)=FHUM(j)-(FBIO3(j)+FHUM3(j))


         CHFBI3(j)=FBIO3(j)*rmW1(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.66*0.00278))*(1-(0.85/(1+rE(j))))
         if(hgr(j) < -20)CHFBI3(j)=0

         CHFHU3(j)=FHUM3(j)*rmW1(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.02*0.00278))*(1-0.85*(0.85/(1+rE(j))))

         if(hgr(j) < -20)CHFHU3(j)=0

c==============================================================
c  Summarnoe razlogenie po vsem komponentam udobreniy
c============================================================
         BIOfum(j)=FDBIO(j)+FBIO1(j)+FBIO3(j)
         HUMfum(j)=FDHUM(j)+FHUM1(j)+FHUM3(j)

	      SMCfum(j)=(BIOfum(j)+HUMfum(j))*10

         SMNfum(j)=SMCfum(j)*1000/rnitr(5)

c        if(j.gt.1) SMNfum(j)=SMCfum(j)/CNfum(j)  

         CO2fum(j)=((FDCO(j)+FR1CO(j)+FR3CO(j))*10)

         


	      SMCOfm=SMCOfm+CO2fum(j)
        
c        CHBIfm(j)=CHFBI0(j)*usl3(j)*30+CHFBI1(j)*usl3(j)*30+CHFBI3(j)
c     4 *usl3(j)*30
c        CHHUfm(j)=CHFHU0(j)*usl3(j)*30+CHFHU1(j)*usl3(j)*30+CHFHU3(j)
c     4 *usl3(j)*30


         CHBIfm(j)=(CHFBI0(j)+CHFBI1(j)+CHFBI3(j))*10
 
         
        
         CHHUfm(j)=(CHFHU0(j)+CHFHU1(j)+CHFHU3(j))*10
         

         CHfum(j)=CHBIfm(j)+CHHUfm(j)
c        If(usl3(j).eq.0) CHfum(j)=0
c        If(usl3(j).eq.1) CHfum(j)=(CHBIfm(j)+CHHUfm(j))*0.33
c        If(usl3(j).eq.2) CHfum(j)=(CHBIfm(j)+CHHUfm(j))*0.66
c        If(usl3(j).eq.3) CHfum(j)=(CHBIfm(j)+CHHUfm(j))*1.00




         SMCHfm=SMCHfm+CHfum(j)
c========================================================
c============================================================
c  Nnakoplenie ugleroda na pole i vibrosi CO2: PolcC(j) i  PolCO2(j)
c=================================================================
	      PoleC(j)=(BIOrst(j)+HUMrst(j)+BIOsoi(j)+HUMsoi(j)+BIOfum(j)+HUMfum(j))*10
        
ccccc        PolCO2(j)=CO2rst(j)+CO2soi(j)+CO2fum(j)
cccc       PolCO2(j)
         SPolCO=SMCOrs+SMCOso+SMCOfm
c==============================================================
c            GODOVIE  SUMMI
c==============================================================
c     RASCHET ZA GOD RAZLOGJENIJ  otdelnix komponentov RASTITELNIX OSTATKOV
c             i  otnoschenij  C/N
c==============================================================
         SmDBIO=SmDBIO+DBIO(j)*10+PrDBIO(j)*10
         SmBIO1=SmBIO1+BIO1(j)*10+PrBIO1(j)*10
         SmBIO2=SmBIO2+BIO2(j)*10+PrBIO2(j)*10
         SmBIO3=SmBIO3+BIO3(j)*10+PrBIO3(j)*10

         SmDHUM=SmDHum+DHUM(j)*10+PrDHUM(j)*10
         SmHUM1=SmHum1+HUM1(j)*10+PrHUM1(j)*10
         SmHUM2=SmHum2+HUM2(j)*10+PrHUM2(j)*10
         SmHUM3=SmHum3+HUM3(j)*10+PrHUM3(j)*10

         TSMCrs=TSMCrs+SMCrst(j)
         TSMNrs=TSMNrs+SMNrst(j)
ccc&&&&&&         SMNrst(j)=rnitr(3)
         OCNrst=rnitr(3)
c         if(j.gt.1) SMNrst(j)=SMCrst(j)/CNrst(j)
ccccccccccc          if(j.gt.1) OCNrst=SMCrst(j)/CNrst(j)  
ccccccccccccc	  OCNrst=SMCrst(j)/CNrst(j)

ccccccccc         CNrst(j)=TSMCrs/TSMNrs

         SmDCO=SmDCO+(DCO2(j)+PrDCO2(j))*10
         SmR1CO=SmR1CO+(R1CO2(j)+PrR1CO(j))*10
         SmR2CO=SmR2CO+(R2CO2(j)+PrR2CO(j))*10
         SmR3CO=SmR3CO+(R3CO(j)+PrR3CO(j))*10


c	 if(j.gt.1)go to 2001
cccccccccccccc	SMNrst(j)=SMCrst(j)*1000/rnitr(3)*10

	      SMNrst(j)=(SMCrst(j)*1000/rnitr(3))*10


c	 go to 2002
c 2001     continue
ccc	CNrst(j)=TSMCrs/TSMNrs
c        OCNrst=SMCrst(j)/CNrst(j)
cccc	SMNrst(j)=SMCrst(j)/CNrst(j)
ccc	SMNrst(j)=SMCrst(j)/OCNrst
ccc        SMNrst(j)=OCNrst/CNrst(j)
    

ccc	SMNrst(j)=SMCrst(j)/rnitr(3)
ccccccccc         if(j.eq.1) go to 22
ccccccccccc   22     OCNrst=SMCrst(j)/rnitr(3)
c         if(j.gt.1) SMNrst(j)=SMCrst(j)/CNrst(j)
ccccccccccc          if(j.gt.1) goto 23
ccccccccccc  23      OCNrst=SMCrst(j)/CNrst(j)  
ccccccccccccc	  OCNrst=SMCrst(j)/CNrst(j)

ccccccccc         CNrst(j)=TSMCrs/TSMNrs
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c        if(j.gt.1)go to 2001
c        epot(j)=0.5*dww(j)*dv(j)*0.75
c        x12(j)=(ts2/inf(6))
c        betxr=0.89554-1.2546*x12(j)+20.303*(x12(j))**2-60.042*(x12(j))
c     2**3+65.887*(x12(j))**4-24.840*(x12(j))**5
c        vgr(j)=epot(j)/exp(1.55*hgr(j))
c        exr=16.7*((inf(30)*qxr)-(dv(j)*inf(31)*0.001)) 
c        eakxr=(2*inf(46)+(os(j)+pnor(j)))/
c     1 (1+(2*(inf(7)-inf(47)))/(betxr*exr))
cc        IF(EAKXR.GT.exr)EAKXR=exr
c        if(eakxr.lt.(0.2*exr))eakxr=0.2*exr
c        filt(j)=inf(46)+os(j)+pnor(j)-inf(7)-eakxr
c        if(filt(j).lt.0.)filt(j)=0
c        w0(j)=inf(46)+os(j)+pnor(j)-eakxr-filt(j)+vgr(j)
c        if(hgr(j).eq.0)w0(j)=inf(7)
c       go to 2002
cc     go to 2003
c 2001     continue
c        epot(j)=0.5*dww(j)*dv(j)*0.75
c        x12(j)=(ts2/inf(6))
c        betxr=0.89554-1.2546*x12(j)+20.303*(x12(j))**2-60.042*(x12(j))
c     2**3+65.887*(x12(j))**4-24.840*(x12(j))**5
c        vgr(j)=epot(j)/exp(1.55*hgr(j))
c        exr=16.7*((inf(30)*qxr)-(dv(j)*inf(31)*0.001)) 
c        eakxr=(2*w0(j-1)+(os(j))+pnor(j))/
c     1(1+(2*(inf(7)-inf(47)))/(betxr*exr))
cc         IF(EAKXR.GT.EXR)EAKXR=EXR
c         if(eakxr.lt.(0.2*exr))eakxr=0.2*exr
c        filt(j)=w0(j-1)+os(j)+pnor(j)-inf(7)-eakxr
c        if(filt(j).lt.0.)filt(j)=0
c        w0(j)=w0(j-1)+os(j)+pnor(j)-eakxr-filt(j)+vgr(j)
c        if(hgr(j).eq.0)w0(j)=inf(7)
cc RASCHET FUNKZIY VLIJNIJ UVLAGJNENIJ PO RASCHITANNOY VLAGJNOSTI POCHVI
c 2002     x11(j)=(ts2/inf(6))
c       if(x11(j).lt.0.1) Wtp=0.65
c       if(x11(j).gt.0.1.and.x11(j).lt.0.75) Wtp=0.75
c       if(x11(j).gt.0.75) Wtp=0.65
c       wtopt2=inf(7)
c       xw1=W0(j)/(wtp*inf(7))
c      xw2=W0(j)/Wtopt2 
c      if(xw2.gt.1.1)xw2=1.1
c      if(W0(j).gt.Wtopt2)gamf=-0.654+3.824*xw2-2.633*(xw2**2)+0.467*
c     7(xw2**3)

c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c==============================================================
c  RASCHET ZA GOD RAZLOGJENIJ otdelnix komponentov ORGANICHESKOGO VESCHESTVA POCHVI
c                i  otnoschenij  C/N
c===============================================================
 2002    SsDBIO=SsDBIO+SDBIO(j)*10
         SsBIO1=SsBIO1+SBIO1(j)*10
         SsBIO2=SsBIO2+SBIO2(j)*10
         SsBIO3=SsBIO3+SBIO3(j)*10
   
         SsDHUM=SsDHUM+SDHUM(j)*10
         SsHUM1=SsHUM1+SHUM1(j)*10
         SsHUM2=SsHUM2+SHUM2(j)*10
         SsHUM3=SsHUM3+SHUM3(j)*10
   
         TSMCso=TSMCso+SMCsoi(j)
         TSMNso=TSMNso+SMNsoi(j)
cccc       CNsoil(j)=TSMCso/TSMNso
         CNsoil(j)=rNitr(4)
         SsDCO=SsDCO+SDCO(j)*10
         SsR1CO=SsR1CO+sR1CO(j)*10
         SsR2CO=SsR2CO+sR2CO(j)*10
         SsR3CO=SsR3CO+sR3CO(j)*10

c==============================================================
c  RASCHET ZA GOD RAZLOGJENIJ otdelnix  komponentov  VESCHESTVA 
c  ORGANICHESKIX UDOBRENIY i  otnoschenij  C/N
c===============================================================
         SFDBIO=SFDBIO+FDBIO(j)
         SFBIO1=SFBIO1+FBIO1(j)

         SFBIO3=SFBIO3+FBIO3(j)

         SFDHUM=SFDHUM+FDHUM(j)
         SFHUM1=SFHUM1+FHUM1(j)

         SFHUM3=SFHUM3+FHUM3(j)

         TSMCfm=TSMCfm+SMCfum(j)
         TSMNfm=TSMNfm+SMNfum(j)
cccc       CNfum(j)=TSMCfm/TSMNfm
         CNfum(j)=rNitr(5)
       
         SFDCO=SFDCO+FDCO(j)*10
         SFR1CO=SFR1CO+FR1CO(j)*10

         SFR3CO=SFR3CO+FR3CO(j)*10

c==============================================================
c  RASCHET ZA GOD  otnoschenij  C/N  summarnogo
c===============================================================

	      CNsum(j)=(TSMCrs+TSMCso+TSMCfm)/(TSMNrs+TSMNso+TSMNfm)

c==============================================================
c RASCHET  NITRIFIKAZII  I EMISSII AZOTA V PROZESSE  NITRIFIKAZII
c==============================================================
c vrem          mt2(36),mW2(36),mpH2(36)
c	VNnitr(j)=rnitr(1)*exp(-0.6*rmt2(j)*rmW2(j)*rmpH2(j))
c        if(Tpoch(j).lt.0.or.Tpoch(j).eq.0) VNnitr(j)=0

ccccccccccccccc        rMNsoi(j)=(inf(19)*inf(8)*10)*rnitr(1)


c  osn ccccccccccccccccccccccccc        rMNsoi(j)=((inf(19)*inf(8)*1000)*rnitr(1))/10000

         rMNsoi(j)=(((inf(19)*inf(8)*1000000)*rnitr(1))/100)/1000000
c          * na 1000000 perevod v grammi
c         /100 umnogjili na mg/100 g
c           /1000000   perevod mg v kg


cccccccccccc        rMNsoi(j)=(inf(19)*inf(8)*1000)/rnitr(1)*10000

c                 kg N / ga

cccccccccccccc	VNnitr(j)=rMNsoi(j)*exp(-0.6*rmt2(j)*rmW2(j)*rmpH2(j))+
ccccccccccccc     4 SMNrst(j)+SMNsoi(j)+SMNfum(j)+rnitr(7)*rnitr(8)
cccccccccccc        if(Tpoch(j).lt.0.or.Tpoch(j).eq.0) VNnitr(j)=0


cccccccccc      VNnitr(j)=(rMNsoi(j)+(SMNrst(j)+SMNsoi(j)+SMNfum(j))*0.7)*exp(-0.6
cccccccccccc     4   *rmt2(j)*rmW2(j)*rmpH2(j))+rnitr(7)*rnitr(8)
ccccccccccccc        if(Tpoch(j).lt.0.or.Tpoch(j).eq.0) VNnitr(j)=0

         RNupt(j)=0.15*(CUrost(j)/rnitr(3))*1000*rnitr(11)


        VNnitr(j)=(rMNsoi(j)+(SMNrst(j)+SMNsoi(j)+SMNfum(j)-RNupt(j))*0.8)*exp(-0.6*rmt2(j)*rmW2(j)*rmpH2(j))+rnitr(7)*rnitr(8)
        if(Tpoch(j).lt.0.or.Tpoch(j).eq.0) VNnitr(j)=0




c      Primechanie  20% visvobogjdaemogo azota vkljuchaetsj v stabilhuju funkziju organicheskogo veschestva
c           str/258 Gollandzi


c	VNnitr(j)=(rnitr(1)*exp(-0.6*rmt2(j)*rmW2(j)*rmpH2(j))+
c     4 SMNrst(j)+SMNsoi(j)+SMNfum(j)+rnitr(7)*rnitr(8))*30
c        if(Tpoch(j).lt.0.or.Tpoch(j).eq.0) VNnitr(j)=0

c   	VNnitr(j)=NNH4

ccccccccc        VNN2O(j)=rmt2(j)*rmW2(j)*rmpH2(j)*(0.004+0.030*rMNsoi(j))

         VNN2O(j)=(rmt2(j)*rmW2(j)*rmpH2(j)*(0.004+0.030*VNnitr(j)))*10

        
         
   
         SnN2O=SnN2O+VNN2O(j)

cccc  vrem        VNN2O(j)=((0.2*Whgr(j)/inf(17))+(0.02*(1-0.1)))*rMNsoi(j)
cccccc        VNNO(j)=0.02*0.1*rnitr(1)

         VNNO(j)=0.02*0.1*VNnitr(j)   

c==============================================================
c RASCHET  DENITRIFIKAZII  I EMISSII AZOTA V PROZESSE  DENITRIFIKAZII
c==============================================================
c  denNO3(36),  denW(36),denCO2(36),denpW(36),dnpNO3(36)

	      VNdeni(j)=denNO3(j)*denW(j)*denCO2(j)*VNnitr(j)
c-----------------------------------------------------------
c	rNmicr(j)=(VNnitr(j)*1000000000)/(inf(19)*0.2*10000*1000000)
	      rNmicr(j)=VNnitr(j)/(inf(19)*inf(8)*10)

ccccccccccccccc       rNmicr(j)=VNnitr(j)*10000000000/(inf(19)*inf(8)*1000000*10000)

         FdNO3(j)=11.0+((40.0*atan(3.14*0.002*(rNmicr(j)-180)))/3.14)

c            gramm N / ga v sutki 

cccccccccccc        FdNO3(j)=(11.0+((40.0*atan(3.14*0.002*(rNmicr(j)-180)))
cccccccccccc     4 /3.14))/1000000000


         FdWFPS(j)=4.82/14**(16/14**(1.39*rchW2(j)))
         if(FdWFPS(j).gt.1.0)FdWFPS(j)=1.0   
cccccccccccccccccccc        vNdN2(j)=(FdNO3(j)*FdWFPS(j)*30)/1000

         VNdN2(j)=(FdNO3(j)*FdWFPS(j)*10)/1000

         


c   perevod iz grammov  v vkilogrammi
  
cccccccccccccc	vNdN2(j)=RNNO3*FdWFPS(j)
         SdN2O=SdN2O+VNdN2(j)         
         SN2O=SnN2O+SdN2O   
c-----------------------------------------------------------
ccc vr        VNdN2(j)=denpW(j)*dnpNO3(j)*VNdeni(j)
ccc   vrem     VNdN2O(j)=(1-(denpW(j)*dnpNO3(j)))*VNdeni(j) 

cccccccccccccc		dFNO3(j)=11.0+(40.0+atan(3.14*0.002*(

cc              VNdN2(j)=VNnitr(j)*1000000000/300*1000000
c================================rchW2(j))===================================
c  RASCHET VIVETRIVANNOSTI AMMONIJ PRI VNESENII ORGANICHESKIX
c             I MINERALNIX UDOBRENIY
c===================================================================

            if(Os(j).lt.rnitr(9))VNvfum(j)=0.15*0.2*inf(9)
            if(Os(j).gt.rnitr(9))VNvfum(j)=0 
           
            if(Os(j).lt.rnitr(9))VNvfrt(j)=0.15*rnitr(8)*rnitr(7) 
	         if(Os(j).gt.rnitr(9))VNvfrt(j)=0


c==============================================================
c  RASCHET VINIOSA AZOTA ZA SCHET INFILTRAZII
c==============================================================
	         Winfil(j)=os(j)-Tisp(j)-(inf(17)-inf(16))
            if(Winfil(j).eq.0.or.Winfil(j).lt.0)Winfil(j)=0
  
	         RNinfl(j)=NNO3*(1-exp(-1.2*(Winfil(j)/10*inf92)))
            if(Winfil(j).eq.0)RNinfl(j)=0


c==============================================================
c  RASCHET    IMMOBILIZAZII     AZOTA (poka ne rassmatrivaem)
c==============================================================
cccccc	Nimmob(j)=(SMCrst(j)+SMCsoi(j)+SMCfum(j))/CNsum(j)-


ccccccccccc(0.85)*(0.85/(1+(1/ratX(j))))

c==============================================================
c  RASCHET    POGLOSCHENIJ     AZOTA   RASTENIEM
c==============================================================
c==================================================================
c       RASCHET  RASTITELNIX  OSTATKOV
c==================================================================
c       Ur=inf(6)
c  inf(5) - chislo mesjzev vegetazii
c       if(inf(4).eq.1)SumRst=(0.48*Ur+2.0)
c==================================================================
c      RASCHET RASPREDELENIJ RASTITELNIX OSTATKOV PO MESJZAM VEGETAZII
c===================================================================
c          "inf(7) - nomer pervogo mesjza vegetazii kulturi"

c       CUrost(j)=((2.3026*(2./3)*10.**(2.-(2./3)*
c     6 (j-inf(7)+1)))/(1.+10.**(2.-(2./3)*(j-inf(7)+1)))**2)*SumRst

c       if(j.lt.inf(7))CUrost(j)=0
c       if(j.gt.(inf(7)+inf(5)))CUrost(j)=0

cc================================================================






         OtnUrs(j)=((2.3026*(2./3)*10.**(2.-(2./3)*((j-inf(7)+1)/3)))/(1.+10.**(2.-(2./3)*((j-inf(7)+1)/3)))**2)
         
         if(j.lt.inf(7))OtnUrs(j)=0
         if(j.gt.(inf(7)+inf(5)))OtnUrs(j)=0
       
c        if(OtnUrs(j).gt.0) RNupt(j)=OtnUrs(j)*Ur*0.035*1.05*1.35

ccccccccccccc       if(inf(10).eq.1)	RNupt(j)=(OtnUrs(j)*(inf(11)+(inf(11)/inf(13)))
ccccccccccc     4 *0.005*1.05*1.00)*30
ccccccccccccccccccc      if(inf(10).eq.1)RNupt(j)=(OtnUrs(j)*(inf(11)+(inf(11)/inf(13))))/
cccccccccccc     4 rnitr(3)
cccccccccccccccccccccccccccccccccccccccccccccc      RNupt(j)=(CUrost(j)/rnitr(3))


cccc       if(inf(4).eq.1)	RNupt(j)=OtnUrs(j)*Ur*0.035*1.05*1.35
cccccccccc       if(inf(4).eq.2)	RNupt(j)=OtnUrs(j)*Ur*0.030*1.05*1.23
cccccccc       if(inf(4).eq.3)	RNupt(j)=OtnUrs(j)*Ur*0.016*1.05*1.2
cccccccc       if(inf(4).eq.4)	RNupt(j)=OtnUrs(j)*Ur*0.035*1.05*1.56
ccccccccc       if(inf(4).eq.5)	RNupt(j)=OtnUrs(j)*Ur*0.030*1.05*1.47
cccccccccc       if(inf(4).eq.6)	RNupt(j)=OtnUrs(j)*Ur*0.037*1.05*1.40
ccccccc       if(inf(4).eq.7)	RNupt(j)=OtnUrs(j)*Ur*0.005*1.05*1.00
cccccccccc       if(inf(4).eq.0)	RNupt(j)=OtnUrs(j)*Ur*0.000*1.05*0.00

cccc      if(inf(10).eq.1)SumRst=inf(12)*inf(11)+(1-inf(13))*inf(11)*inf(14)
c       if(inf(10).eq.1)	RNupt(j)=OtnUrs(j)*Ur*0.005*1.05*1.00
ccccccccc       if(inf(10).eq.1)	RNupt(j)=OtnUrs(j)*(inf(11)+(inf(11)/inf(13)))
ccccccccccc     4 *0.005*1.05*1.00
c===================================================================
c  BALANS  AMMONIJ  I  NITRATOV  V  POCHVE
c===================================================================

c	NNH4=NNH4+SMNrst(j)+SMNsoi(j)+SMNfum(j)+rnitr(7)*rnitr(8)
c     4     -rnitr(10)*Nupt(j)-VNN2O(j)-vNNO(j)-VNvfum(j)-Vnvfrt(j)

ccccccc	NNH4=NNH4+SMNrst(j)+SMNsoi(j)+SMNfum(j)+rnitr(7)*rnitr(8)
ccccccccccc     4     -rnitr(10)*RNupt(j)-VNN2O(j)-vNNO(j)-VNvfum(j)-Vnvfrt(j)

ccc	NNH4=NNH4+SMNrst(j)+SMNsoi(j)+SMNfum(j)+rnitr(7)*rnitr(8)/1000
ccc  4     -rnitr(10)*RNupt(j)-VNN2O(j)-vNNO(j)-VNvfum(j)-Vnvfrt(j)

ccccccccccccccccccccccccccccccccccccc	NNH4=NNH4+SMNrst(j)+SMNsoi(j)+SMNfum(j)
cccccccccccccccccccccccccccccccccc crnitr(7)*rnitr(8)/1000
cccccccccccccccccccccccccccccccccc c 4     -rnitr(10)*RNupt(j)-VNN2O(j)-vNNO(j)-VNvfum(j)-Vnvfrt(j)

c	NNH4=NNH4+SMNrst(j)+SMNsoi(j)+SMNfum(j)+rnitr(7)*rnitr(8)
c     4   -rnitr(10)*RNupt(j)-VNN2O(j)-vNNO(j)-VNvfum(j)-Vnvfrt(j)
         RNNH4=RNNH4+VNN2O(j)*10
c	BNNH4(j)=NNH4+SMNrst(j)+SMNsoi(j)+SMNfum(j)+rnitr(7)
c     4 *rnitr(8)
c     4   -rnitr(10)*RNupt(j)-VNN2O(j)-vNNO(j)-VNvfum(j)-Vnvfrt(j)




c	NNH4=NNH4+10
c        NNO3=NNO3+VNnitr(j)-VNdeni(j)-rnitr(11)*RNupt(j)-VNdN2(j)-
c     4   VNdN2O(j)-RNinfl(j)
	      RNNO3=RNNO3+(vNdN2(j)*10/1000)
        
c===================================================================
c  BALANS  SUMARNOY  EMISSII ZAKISI  AZOTA
c===================================================================

ccccccccccc	TMNN2O=TMNN2O+VNN2O(j)+VNdN2(j)
		   TMNN2O=SnN2O+SdN2O

c==============================================================
c  RASCHET ZA GOD RAZLOGJENIJ RASTITELNIX  OSTSTKOV
c===============================================================
         RtBIO=SmDBIO+SmBIO1+SmBIO2+SmBIO3
         RtHUM=SmDHUM+SmHUM1+SmHUM2+SMHUM3
         RtCO=SmDCO+SmR1CO+SmR2CO+SmR3CO
c==============================================================
c  RASCHET ZA GOD RAZLOGJENIJ ORGANICHESKOGO VESCHESTVA POCHVI
c===============================================================
         RslBIO=SsDBIO+SsBIO1+SsBIO2+SsBIO3
         RslHUM=SsDHUM+SsHUM1+SsHUM2+SsHUM3
         RslCO=SsDCO+SsR1CO+SsR2CO+SsR3CO
c==============================================================
c  RASCHET ZA GOD RAZLOGJENIJ  VESCHESTVA ORGANICHESKIX  UDOBRENIY
c===============================================================
         RFBIO=SFDBIO+SFBIO1+SFBIO3
         RFHUM=SFDHUM+SFHUM1+SFHUM3
         RFCO=SFDCO+SFR1CO+SFR3CO
c==============================================================
c  RASCHET ZA GOD RAZLOGJENIJ VSEX  RASTITELNIX  OSTSTKOV, ORGANIKI
c  POCHVI  I  VESCHESTVA ORGANICHESKIX  UDOBRENIY
c===============================================================
         SumBIO=RtBIO+RtHUM
         SumHUM=RslBIO+RslHUM
         SSSHUM=RFBIO+RFHUM
         SumCO=RtCO+RslCO+RFCO
c=============================================================
c   RASCHET NERAZLOGIVSHIXSJ  OSTATKOV  DPM  I  RTM
c=============================================================
	      RazDPM=RazDPM+DPM(j)
         RazRPM=RazRPM+RPM(j)
         SumDPM=SumRst-RazDPM-RazRPM

c=================================================================
c  RASCHET  ZA  GOD VIDELENIJ  METANA
c================================================================
	      rastCH=rastCH+CHrst(j)
         soilCH=soilCH+CHsoil(j)
	      fumCH=fumCH+CHfum(j)
c        SummarnoeWidilenieMetana=SummarnoeWidilenieMetana+rastCH+soilCH+fumCH
         SummarnoeWidilenieMetana=SummarnoeWidilenieMetana+CHrst(j)+CHsoil(j)+CHfum(j)    
c===============================================================
c  BALANS UGLERODA  V POCHVE NA POLE  PROEKTA
c===============================================================
         if(inf(4).eq.1)RKwin=0.035
         if(inf(4).eq.2)RKwin=0.030
         if(inf(4).eq.3)RKwin=0.016
         if(inf(4).eq.4)RKwin=0.035
         if(inf(4).eq.5)RKwin=0.003
         if(inf(4).eq.6)RKwin=0.037
         if(inf(4).eq.7)RKwin=0.005
         if(inf(4).eq.0)RKwin=0.0
	
c       write(OutputFileUnit, *) "inf(10) - otnositelniy vinos azota s urogjaem "
c       write(OutputFileUnit, *) "inf(4) = :1- ozim pscheniza (0.035),2 - jachmen(0.030) "
c       write(OutputFileUnit, *) " 3 - gorox (0.016)4 - kukuruza na zerno (0.030)
c      5  5 - kukuruza na silos (0.003)"
c       write(OutputFileUnit, *) "6 - podsolnechnic(0.037)  7 - ljuzerna (0.005)
c      5 , 0 -  par"

         if(inf(4).eq.1)RKwin=0.035
         if(inf(4).eq.2)RKwin=0.030
         if(inf(4).eq.3)RKwin=0.016
         if(inf(4).eq.4)RKwin=0.035
         if(inf(4).eq.5)RKwin=0.003
         if(inf(4).eq.6)RKwin=0.037
         if(inf(4).eq.7)RKwin=0.005
         if(inf(4).eq.0)RKwin=0.0


ccc       if(inf(10).eq.1)PKpoch=0.8
ccc       if(inf(10).eq.2)PKpoch=1.0
ccc       if(inf(10).eq.3)PKpoch=1.6
ccc       if(inf(10).eq.4)PKpoch=1.4
ccc       if(inf(10).eq.5)PKpoch=1.6

c       write(OutputFileUnit, *) "inf(10)= 1-tjgjeliy suglinok; 2- sredniy suglinok:
c      3 3 legkiy suglinok; 4- supeschanaj pochva; 5 peschanaj pochva"

         if(inf(4).eq.1)PKcult=1.35
         if(inf(4).eq.2)PKcult=1.23
         if(inf(4).eq.3)PKcult=1.2
         if(inf(4).eq.4)PKcult=1.56
         if(inf(4).eq.5)PKcult=1.47
         if(inf(4).eq.6)PKcult=1.39
         if(inf(4).eq.7)PKcult=1.0
         if(inf(4).eq.0)PKcult=0.0

c	RWin=10*inf(6)*RKwin*1.0*PKcult


c	RWin=10*inf(6)*RKwin*1.0*PKcult

         if(inf(4).eq.1)	RWin=10*inf(6)*0.035*1.05*1.35
         if(inf(4).eq.2)	RWin=10*inf(6)*0.030*1.05*1.23
         if(inf(4).eq.3)	RWin=10*inf(6)*0.016*1.05*1.2
         if(inf(4).eq.4)	RWin=10*inf(6)*0.035*1.05*1.56
         if(inf(4).eq.5)	RWin=10*inf(6)*0.030*1.05*1.47
         if(inf(4).eq.6)	RWin=10*inf(6)*0.037*1.05*1.40
         if(inf(4).eq.7)	RWin=10*inf(6)*0.005*1.05*1.00
         if(inf(4).eq.0)	RWin=10*inf(6)*0.000*1.05*0.00



c          BalanC=inf(8)+SumBIO+SumHUM+SSSHUM+0.07*inf(1)-RWin-RslCO
ccccccccc           BalanC=inf(8)+SumBIO+SumHUM+SSSHUM-RWin-RslCO
         BalanC=rMCsoi(j)+SumBIO+SumHUM+SSSHUM-RWin-RslCO
         j1m(j)=j
         gim(j)=gi
         ts2m(j)=ts2
         dw51(j)=SumOs
         rd42(j)=rd41
c+++++++++++++++++++
         rd1(j)=SmDBIO
         rd2(j)=SmBIO1
         rd3(j)=SmBIO2
         rd4(j)=SmBIO3
         rd5(j)=SmDHUM
         rd6(j)=SmHUM1
         rd7(j)=SmHUM2
         rd8(j)=SmHUM3
         rd9(j)=SmDCO
         rd10(j)=SmR1CO
         rd11(j)=SmR2CO
         rd12(j)=SmR3CO
c+++++++++++++++++

c++++++++++++++
         rds1(j)=SsDBIO
         rds2(j)=SsBIO1
         rds3(j)=SsBIO2
         rds4(j)=SsBIO3
         rds5(j)=SsDHUM
         rds6(j)=SsHUM1
         rds7(j)=SsHUM2
         rds8(j)=SsHUM3
         rds9(j)=SsDCO
         rds10(j)=SsR1CO
         rds11(j)=SsR2CO
         rds12(j)=SsR3CO
c+++++++++++++++++
c+++++++++++++++++++
         rdf1(j)=SFDBIO
         rdf2(j)=SFBIO1
         rdf3(j)=SFBIO3
         rdf4(j)=SFDHUM
         rdf5(j)=SFHUM1
         rdf6(j)=SFHUM3
         rdf7(j)=SFDCO
         rdf8(j)=SFR1CO
         rdf9(j)=SFR3CO
c+++++++++++++++++
c+++++++++++++++++
         rdg1(j)=RtBIO
         rdg2(j)=RtHUM
         rdg3(j)=RtCO
         rdg4(j)=RslBIO
         rdg5(j)=RslHUM
         rdg6(j)=RslCO
         rdg7(j)=RFBIO
         rdg8(j)=RFHUM
         rdg9(j)=RFCO
         rdg10(j)=SumBIO
         rdg11(j)=SumHUM
         rdg12(j)=SumCO
         drww1(j)=prSMCO
c++++++++++++++++++++++++++++++++++
         rdg13(j)=BalanC
c        rdg14(j)=RIOM
         rmg1(j)=SSSHUM
         rmg2(j)=RRSHUM 
         rmgw1(j)=RWin 
         rmgw2(j)=RazDPM
         rmgw3(j)=RazRPM
         rmgw4(j)=SumDPM
         rmgw5(j)=SumRPM
c        rgdmn1(j)=RslCO
c==================================
         rCrst(j)=TSMCrs
         rCsoil(j)=TSMCso
         rCfum(j)=TSMCfm

         rNrst(j)=TSMNrs
         rNsoil(j)=TSMNso
         rNfum(j)=TSMNfm

         rdCHrs(j)=SMCHrs
	      rdCHso(j)=SMCHso
         rdCHfm(j)=SMCHfm

         rdrsCH(j)=rastCH
         rdsoCH(j)=soilCH
         rdfmCH(j)=fumCH
         summMetanMonths(j)=CHrst(j)+CHsoil(j)+CHfum(j)
      	 rOCNrs(j)=OCNrst
       
         rdNH4(j)=RNNH4
         rdNO3(j)=RNNO3
         rdNN20(j)=TMNN20   
         gn1(j)=SnN2O
         gn2(j)=SdN2O   
         gn3(j)=SN2O
	       gn4(j)=SMCOrs
         gn5(j)=SMCOso
         gn6(j)=SMCOfm
         gn7(j)=SPolCO 	

c+++++++++++++++++++++++++++++++++++++++++++
ccc       rdg11(j)=(rdg2(j)+rdg5(j)+rdg8(j))
ccc       RRSHUM=RRSHUM+rdg11(j)
         j1=j1+1
      end do
      
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      print *, "N2O", gn3(36)
      
      
      j1=j-1

      do iter = 1,n
         dekadesReal(iter) = iter
         gimReal(iter) = gim(iter)
      end do

c      do iter = 1, 36
c         tmpArray(iter) = rMNsoi(j)+(SMNrst(j)+SMNsoi(j)+SMNfum(j)-RNupt(j))
c         tmpArray2(iter) = exp(-rmt1(iter)*rmpH1(iter)*0.6*0.02*0.00278)
c      end do
cc     rMNsoi(j)+(SMNrst(j)+SMNsoi(j)+SMNfum(j)-RNupt(j)
c      call WriteTable(6, "N2O", 
c     > "dek", dekadesReal,"rmw2", rmw2, "rmt2", rmt2, "rMNsoi", rMNsoi, "SMNrst",SMNrst,"SMNsoi",SMNsoi,"SMNfum",SMNfum,"RNupt",RNupt, "Val", tmpArray)

      print *, "Summarnie za god wibrosy", SummarnoeWidilenieMetana * 1000

      call WriteTable(OutputFileUnit,"RASCHET  DEFIZITA  WLAGI",
     >   "dek", dekadesReal,"cyt",gimReal,"Tisp",Tisp,"ratX",ratX,"pBIO",pBIO,"pHUM",pHUM," pCO2",pCO2,"rE",rE)
c=================
      call WriteTable(OutputFileUnit,"RASCHET  DEFIZITA  WLAGI",
     >   "dek", dekadesReal,"cyt",gimReal,"hgr", hgr, "Whgr", Whgr, "rmpH", rmpH,"rmpH1", rmpH1, "rmW", rmW, "rmW1", rmW1, "rmW2", rmW2, "rmW3", rmW3)



c====================================================== 
      call WriteTable(OutputFileUnit, "T A B L I Z A  R.2a",
     >   "dek", dekadesReal,"cyt",gimReal,"hgr", hgr, "rchW2", rchW2,"rmt1",rmt1,"rmt2",rmt2, "rmW2",rmW2,"rmpH2",rmpH2)
       
c======================================================        
      call WriteTable(OutputFileUnit, "RASCHET  PARAMETROV OSNOVNOGO URAVNENIJ I \n RASTITELNIX OSTATKOV  i DPM i  RPM, t/ga",
     >   "dek", dekadesReal, "cyt", gimReal, "raC",raC,"rbC",rbC,"ratX",ratX,"CUrost",CUrost,"DPM0",DPM0,"RPM0",RPM0)

c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      call WriteTable(OutputFileUnit, "RASCHET  RAZLOGJENIJ   DPM(j) \n RASTITELNIX OSTATKOV, t/ga",
     >   "dek", dekadesReal, "cyt", gimReal, "DBIO",DBIO,"DHUM",DHUM,"DCO2",DCO2,"DPM",DPM)

      call WriteTable(OutputFileUnit, "RASCHET  RAZLOGJENIJ   RPM(j) \n RASTITELNIX OSTATKOV, t/ga",
     >   "dek", dekadesReal, "cyt", gimReal, "BIO1",BIO1,"HUM1",HUM1,"R1CO2",R1CO2,"RPM",RPM)

      do iter = 1, n
         tmpArray(iter) = rdh1(iter) + rdh2(iter)
      end do

      call WriteTable(OutputFileUnit, "RASCHET  RAZLOGJENIJ   BIO1(j) \n RASTITELNIX OSTATKOV, t/ga",
     >   "dek", dekadesReal, "cyt", gimReal, "BIO2",BIO2,"HUM2",HUM2,"R2CO2",R2CO2,"rdh1+rdh2", tmpArray)

c===============================================================
      call WriteTable(OutputFileUnit, "RASCHET  videlenij METANA \n IZ   BIO   RASTITELNIX OSTATKOV, t/ga",
     >   "dek", dekadesReal, "cyt", gimReal, "CHBIO",CHBIO,"CHBIO1",CHBIO1,"CHBIO2",CHBIO2,"CHBIO3", CHBIO3)

c===============================================================
      do iter = 1, n
         tmpArray(iter) = rdh3(iter) + rdh4(iter)
      end do

      call WriteTable(OutputFileUnit, "RASCHET  RAZLOGJENIJ   HUM1(j) \n RASTITELNIX OSTATKOV, t/ga",
     >   "dek", dekadesReal, "cyt", gimReal, "BIO3",BIO3,"HUM3",HUM3,"R3CO2",R3CO,"rdh3+rdh4",tmpArray)
      write(OutputFileUnit,120)
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

c===============================================================
      call WriteTable(OutputFileUnit, "RASCHET  videlenij METANA \n IZ   HUM   RASTITELNIX OSTATKOV, t/ga",
     >   "dek", dekadesReal, "cyt", gimReal, "CHHUM",CHHUM,"CHHUM1",CHHUM,"CHHUM2",CHHUM2,"CHHUM3", CHHUM3)
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        
c===============================================================
      call WriteTable(OutputFileUnit, "RASCHET  videlenij CO2 \n IZ  RASTITELNIX OSTATKOV, t/ga",
     >   "dek", dekadesReal, "cyt", gimReal, "DCO2",DCO2,"R1CO2 ",R1CO2,"R2CO2",R2CO2,"R3CO", R3CO)

c===============================================================
      call WriteTable(OutputFileUnit, "RASCHET  videlenij CO2 \n IZ  RASTITELNIX OSTATKOV, t/ga",
     >   "dek", dekadesReal, "cyt", gimReal, "PrDBIO",PrDBIO,"PrDHUM",PrDHUM,"PrDCO2",PrDCO2,"PrDPM",PrDPM)

      call WriteTable(OutputFileUnit, "RASCHET  RAZLOGENUJ  PrRPM(j) \n IZ  RASTITELNIX OSTATKOV, t/ga",
     >   "dek", dekadesReal, "cyt", gimReal, "PrBIO1",BIO1,"PrHUM1",HUM1,"PrR1CO2",R1CO2,"PrRPM",RPM)

      do iter = 1, n
         tmpArray(iter) = Prrdh1(iter) + Prrdh2(iter)
      end do
      call WriteTable(OutputFileUnit, "RASCHET  RAZLOGJENIJ   PrBIO1(j) \n IZ  RASTITELNIX OSTATKOV, t/ga",
     >   "dek", dekadesReal, "cyt", gimReal, "PrBIO2",PrBIO2,"PrHUM2",PrHUM2,"PrR2CO2",PrR2CO,"Prrdh1+Prrdh2", tmpArray)

      do iter = 1, n
         tmpArray(iter) = Prrdh3(iter) + Prrdh4(iter)
      end do      
      call WriteTable(OutputFileUnit, "RASCHET  RAZLOGJENIJ   PrHUM1(j) \n IZ  RASTITELNIX OSTATKOV, t/ga",
     >   "dek", dekadesReal, "cyt", gimReal, "PrBIO3", PrBIO3, "PrHUM3",PrHUM3,"PrR3CO2",PrR3CO,"Prrdh3+Prrdh4",tmpArray)
        
c===============================================================
      call WriteTable(OutputFileUnit, "RASCHET  videlenij METANA \n IZ  PrBIO  RASTITELNIX OSTATKOV, t/ga",
     >   "dek", dekadesReal, "cyt", gimReal, "PCHBIO",PCHBIO,"PCHB1",PCHB1,"PCHB2",PCHB2,"PCHB3", PCHB3)

c===============================================================
      call WriteTable(OutputFileUnit, "RASCHET  videlenij METANA \n IZ  PrHUM  RASTITELNIX OSTATKOV, t/ga",
     >   "dek", dekadesReal, "cyt", gimReal, "PCHHUM", PCHHUM,"PCHH1",PCHH1,"PCHH2",PCHH2,"PCHH3", PCHH3)

c===============================================================
      call WriteTable(OutputFileUnit, "RASCHET  videlenij METANA \n IZ  IZ  tekuschego i proschlogo goda, t/ga",
     >   "dek", dekadesReal, "cyt", gimReal, "CHBIrs",CHBIrs,"CHHUrs",CHHUrs,"CHrst",CHrst,"SMCHrs", rdCHrs)

c===============================================================
      call WriteTable(OutputFileUnit, "SUMMARNOE   RAZLOGJENIE  PO VSEM KOMPONENTAM \n RASTITELNIX OSTATKOV, t/ga",
     >   "dek", dekadesReal, "cyt", gimReal, "BIOrst",BIOrst,"HUMrst",HUMrst,"CO2rst",CO2rst,"CHBIrs",CHBIrs,"CHHUrs",CHHUrs,"CHrst",CHrst)

c=========================================
      call WriteTable(OutputFileUnit, "VIBROSI CO2  PROSCHLOGO  GODA \n RASTITELNIX OSTATKOV, t/ga",
     >   "dek", dekadesReal, "cyt", gimReal, "prDCO2",prDCO2,"prR1CO",prR1CO,"prR2CO",prR2CO,"prR3CO",prR3CO)

      

c=========================================
      do iter = 1, n
         tmpArray(iter) = inf(8)
      end do 
      call WriteTable(OutputFileUnit, "RASCHET  NACHALNIX ZNACHENIY ORGANIKI \n POCHVI, t/ga",
     >   "dek", dekadesReal, "cyt", gimReal,"SDPM  ",SDPM,"SRPM",SRPM,"SBIO",SBIO,"SHUM",SHUM,"IOM",RIOM,"SOM", tmpArray)

c============================
      call WriteTable(OutputFileUnit, "RASCHET  ROZLOGJENIJ  ORGANIKI \n POCHVI, t/ga",
     >   "dek", dekadesReal, "cyt", gimReal,"SDPM0",SDPM0,"SRPM0",SRPM0,"SBIO0",SBIO0,"SHUM0",SHUM0)
      
c============================
      call WriteTable(OutputFileUnit, "RASCHET  ROZLOGJENIJ  SDPM0 \n POCHVI, t/ga",
     >   "dek", dekadesReal, "cyt", gimReal,"SDBIO  ",SDBIO,"SDHUM",SDHUM,"SDCO2",SDCO)

c============================
      call WriteTable(OutputFileUnit, "RASCHET  ROZLOGJENIJ  SRPM0 \n POCHVI, t/ga",
     >   "dek", dekadesReal, "cyt", gimReal,"SBIO1  ",SBIO1,"SHUM1",SHUM1,"SR1CO2",SR1CO)

c============================
      call WriteTable(OutputFileUnit, "RASCHET  ROZLOGJENIJ  SBIO0(j) \n POCHVI, t/ga",
     >   "dek", dekadesReal, "cyt", gimReal,"SBIO2  ",SBIO2,"SHUM2",SHUM2,"SR2CO2",SR2CO)

c============================
      call WriteTable(OutputFileUnit, "RASCHET  ROZLOGJENIJ  SHUM0(j) \n POCHVI, t/ga",
     >   "dek", dekadesReal, "cyt", gimReal,"SBIO3  ",SBIO3,"SHUM3",SHUM3,"SR3CO2",SR3CO)

c===================================================
      call WriteTable(OutputFileUnit, "RASCHET  videlenij METANA \n IZ BIO POCHVI, t/ga",
     >   "dek", dekadesReal, "cyt", gimReal,"HSBIO0",HSBIO0,"HSBIO",HSBIO,"HSBIO1",HSBIO1,"HSBIO2",HSBIO2,"HSBIO3" ,HSBIO3)
      
c===============================================================
      call WriteTable(OutputFileUnit, "RASCHET  videlenij METANA \n IZ HUM POCHVI, t/ga",
     >   "dek", dekadesReal, "cyt", gimReal,"HHUM00",HHUM00,"HHUM0",HHUM0,"HHUM1",HHUM1,"HHUM2" ,HHUM2,"HHUM3" ,HHUM3)
c===============================================================
      call WriteTable(OutputFileUnit, "SUMMARNOE   RAZLOGJENIE  PO VSEM KOMPONENTAM \n ORGANIKI  POCHVI, t/ga",
     >   "dek", dekadesReal, "cyt", gimReal, "BIOsoil  ",BIOsoi,"HUMsoil",HUMsoi,"CO2soil",CO2soi)

c===============================================================
      call WriteTable(OutputFileUnit, "RASCHET  videlenij METANA SUMMARNO IZ VSEX BIO i HUM POCHVI , t/ga \n I SUMMARNO   IZ VSEY   POCHVI , t/ga",
     >   "dek", dekadesReal, "cyt", gimReal,"CHBIso",CHBIso, "CHHUso",CHHUso,"CHsoil",CHsoil,"SMCHso", rdCHso)

c===============================================================
      call WriteTable(OutputFileUnit, "RASCHET  PERVICHNOGO   ROZLOGJENIJ \n ORGANICHESKIX  UDOBRENIY, t/ga",
     >   "dek", dekadesReal, "cyt", gimReal,"FDPM",FDPM,"FRPM",FRPM,"FHUM",FHUM)

c======================
      call WriteTable(OutputFileUnit, "RASCHET     ROZLOGJENIJ       FDPM(j) \n ORGANICHESKIX  UDOBRENIY, t/ga",
     >   "dek", dekadesReal, "cyt", gimReal,"FDBIO  ",FDBIO,"FDHUM",FDHUM,"FDCO",FDCO)

c======================
      call WriteTable(OutputFileUnit, "RASCHET     ROZLOGJENIJ       FRPM(j) \n ORGANICHESKIX  UDOBRENIY, t/ga",
     >   "dek", dekadesReal, "cyt", gimReal,"FBIO1  ",FBIO1,"FHUM1",FHUM1,"FR1CO",FR1CO)

c======================
      call WriteTable(OutputFileUnit, "RASCHET     ROZLOGJENIJ       FHUM(j) \n ORGANICHESKIX  UDOBRENIY, t/ga",
     >   "dek", dekadesReal, "cyt", gimReal,"FBIO3  ",FBIO3,"FHUM3",FHUM3,"FR3CO",FR3CO)
      
c===============================================================
      call WriteTable(OutputFileUnit, "RASCHET  videlenij METANA \n IZ   BIO I  HUM   UDOBRENIY , t/ga",
     >   "dek", dekadesReal, "cyt", gimReal,"CHFBI0",CHFBI0,"CHFBI1",CHFBI1,"CHFBI3",CHFBI3,"CHFHU0",CHFHU0,"CHFHU1",CHFHU1,"CHFHU3", CHFHU3)

c===============================================================
      call WriteTable(OutputFileUnit, "SUMMARNOE   RAZLOGJENIE  PO VSEM KOMPONENTAM \n ORGANICHESKIX  UDOBRENIY, t/ga",
     >   "dek", dekadesReal, "cyt", gimReal,"BIOfum  ",BIOfum,"HUMfum",HUMfum,"CO2fum",CO2fum)

c===============================================================
      call WriteTable(OutputFileUnit, "RASCHET  videlenij METANA \n SUMMARNOGO  IZ    UDOBRENIY, t/ga",
     >   "dek", dekadesReal, "cyt", gimReal,"CHBIOfm",CHBIfm, "CHHUMfm",CHHUfm,"CHfum",CHfum,"SMCHfum",rdCHfm)

c===============================================================
      call WriteTable(OutputFileUnit, "RASCHET  KOEFFIZIENTOV URAVNENIY \n RASCHETA NITRIFIKAZII  I DENITRIFIKAZII",
     >   "dek", dekadesReal, "cyt", gimReal,"denNO3",denNO3, "denW",denW,"denCO2",denCO2,"denpW",denpW, "dnpNO3", dnpNO3)

c===============================================================
      call WriteTable(OutputFileUnit, "RASCHET  SKOROSII OBRAZOVANIJ UGLERODA",
     >   "dek", dekadesReal, "cyt", gimReal,"SMCrst",SMCrst, "SdN2O",gn2,"SnN2O",gn1,"denpW",denpW,"dnpNO3", dnpNO3)
      
c===============================================================
      call WriteTable(OutputFileUnit, "RASCHET",
     >   "dek", dekadesReal, "cyt", gimReal,"rNmicr",rNmicr,"FdNO3",FdNO3,"FdWFPS",FdWFPS,"rchW2",rchW2)

c===============================================================
      call WriteTable(OutputFileUnit, "RASCHET  SKOROSTI MINERALIZAZII - OBRAZOVANIJ AMMONIJ",
     >   "dek", dekadesReal, "cyt", gimReal,"SMNrst",SMNrst, "SMNsoi",SMNsoi,"SMNfum",SMNfum,"SMNsum" ,SMNfum)

c===============================================================
      call WriteTable(OutputFileUnit, "RASCHET  NITRIFIKAZII  I EMISSII AZOTA V PROZESSE \n NITRIFIKAZII - VNnitr(j),VNN2O(j),VNNO(j)",
     >   "dek", dekadesReal, "cyt", gimReal,"VNnitr",VNnitr, "VNN2O",VNN2O,"VNNO" ,VNNO)

c===============================================================
      call WriteTable(OutputFileUnit, "RASCHET  NITRIFIKAZII  I EMISSII AZOTA V PROZESSE \n DENITRIFIKAZII - VNdeni(j),VNdN2(j),VNdN2O(j)", 
     >   "dek", dekadesReal, "cyt", gimReal,"VNdeni",VNdeni,"VNdN2+4N2O",VNdN2,"VNdN2O", VNdN2O)
c===============================================================
      call WriteTable(OutputFileUnit, "RASCHET  POGLOSHENIJ AZOTA RASTENIJMI \n INFILTRAZII I VIVETRIVANIJ AZOTA, RNupt(j),RNinfl(j),Unvfum(j),Unvfrt(j)", 
     >   "dek", dekadesReal, "cyt", gimReal,"RNupt ",RNupt,"RNinfl",RNinfl,"Vnvfum",Vnvfum,"Vnvfrt", Vnvfrt)

c===============================================================
      call WriteTable(OutputFileUnit, "VIBROSI N2O pri nitrifikazii SumnitrN2O \n VIBROSI N2O pri denitrifikazii SumdenN2O  (kg N / ga za god)", 
     >   "dek", dekadesReal, "cyt", gimReal,"SumnitrN2O",VNN2O,"sumdenN2o",VNdN2,"rMCsoi", rMCsoi)

c===============================================================
      call WriteTable(OutputFileUnit, "RASTENIJMI \n A", 
     >   "dek", dekadesReal, "cyt", gimReal,"SMCOrs ",gn4,"SMCOso",gn5,"SMCOfm",gn6,"SPolCO",gn7)

c===============================================================
      call WriteTable(OutputFileUnit, "RASTENIJMI \n A", 
     >   "dek", dekadesReal, "cyt", gimReal,"CUrost ",CUrost,"RNupt",RNupt,"SMCOfm",gn6,"SPolCO", gn7)

c===============================================================
      call WriteTable(OutputFileUnit, "VIBROSI CO2 \n VIBROSI CO2", 
     >   "dek", dekadesReal, "cyt", gimReal," CO2",rdg12, "Tpoch",Tpoch,"rMCsoi", rMCsoi)

c============================================================
c  Nnakoplenie ugleroda na pole i vibrosi CO2: PolcC(j) i  PolCO2(j)
      call WriteTable(OutputFileUnit, "SUMMARNOE   NAKOLENIE  UGLERODA NA POLE(PoleC(j) \n I  VIBROSI  CO2 (PolCO2(j), t/ga", 
     >   "dek", dekadesReal, "cyt", gimReal,"PoleC  ",PoleC,"PoleCO2",gn7)

c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      

      write(OutputFileUnit, *) "RASCHET ZA GOD RAZLOGJENIJ RASTITELNIX OSTATKOV, t/ga;"


      call WriteItems(OutputFileUnit, "SmDBIO",rd1(n),"SmBIO1",rd2(n)," SmBIO2",rd3(n),
     >   "SmBIO3",rd4(n),"SmDHUM",rd5(n),"SMHUM1",rd6(n),"SMHUM2",rd7(n),"SMHUM3",rd8(n),
     >   "SmDCO",rd9(n))

      call WriteItems(OutputFileUnit,"SMR1CO",rd10(n),"SMR2CO",rd11(n),"SMR3CO",rd12(n))

      write(OutputFileUnit,120)
       write(OutputFileUnit, *) "RASCHET ZA GOD RAZLOGJENIJ ORGANIKI POCHVI, t/ga ;"
      write(OutputFileUnit,120)
      write(OutputFileUnit,878)rds1(n),rds2(n),rds3(n),rds4(n),rds5(n),rds6(n),
     4 rds7(n),rds8(n),rds9(n),rds10(n),rds11(n),rds12(n)
c 827 4 format(1x,"SmDBIO=",f10.5)
878   format(1x,"SsDBIO=",f7.5,1x,"SsBIO1=",f7.5,1x,"SsBIO2=",f7.5,1x,
     4 "SsBIO3=",f7.5,1x,"SsDHUM=",f7.5,1x,"SsHUM1=",f7.5,1x,
     4 "SsHUM2=",f7.5,1x,"SsHUM3=",f7.5,1x,"SsDCO=",f7.5,1x,"SsR1CO="
     4 ,f7.5,5x,
     4 "SsR2CO=",f7.5,1x,"SsR3CO=",f7.5)
      write(OutputFileUnit,120)
      write(OutputFileUnit,120)
       write(OutputFileUnit, *) "RASCHET ZA GOD RAZLOGJENIJ ORGANIKI UDOBRENIY, t/ga;"
      write(OutputFileUnit,120)
      write(OutputFileUnit,879)rdf1(n),rdf2(n),rdf3(n),rdf4(n),rdf5(n),rdf6(n),
     4 rdf7(n),rdf8(n),rdf9(n)
c 827 4 format(1x,"SmDBIO=",f10.5)
 879   format(1x,"SFDBIO=",f7.5,1x,"SFBIO1=",f7.5,1x,
     4 "SFBIO3=",f7.5,1x,"SFDHUM=",f7.5,1x,"SFHUM1=",f7.5,1x,
     4 "SFHUM3=",f7.5,1x,"SFDCO=",f7.5,1x,"SFR1CO="
     4 ,f7.5,5x,"SFR3CO=",f7.5)
      write(OutputFileUnit,120)

      write(OutputFileUnit,120)
      write(OutputFileUnit,120)
       write(OutputFileUnit, *) "RASCHET ZA GOD RAZLOGJENIJ RASTITELNIX OSTATKOV,;"
       write(OutputFileUnit, *) "   VSEX KOMPONENTOV, t/ga ;"
      write(OutputFileUnit,120)
      write(OutputFileUnit,900)rdg1(n),rdg2(n),rdg3(n)

 900   format(1x,"RastBIO =",f7.6,1x,"RastHUM =",f7.6,1x,
     4 "RastCO2 =",f10.8,1x)
      write(OutputFileUnit,120)
      write(OutputFileUnit,120)


      write(OutputFileUnit,120)
      write(OutputFileUnit,120)
       write(OutputFileUnit, *) "RASCHET ZA GOD NERAZLOGIVSHIXSJ RASTIT OSTATKOV, t/ga;"
       write(OutputFileUnit, *) "  ;"
      write(OutputFileUnit,120)
      write(OutputFileUnit,876)rmgw4(n),drww1(j)

 876   format(1x,"SumDPM =",f12.10,1x,"prSMCO =",f12.10,1x)
      write(OutputFileUnit,120)
      write(OutputFileUnit,120)


       write(OutputFileUnit, *) "RASCHET ZA GOD RAZLOGJENIJ ORGANIKI POCHVI,;"
       write(OutputFileUnit, *) "   VSEX KOMPONENTOV, t/ga ;"
      write(OutputFileUnit,120)
      write(OutputFileUnit,901)rdg4(n),rdg5(n),rdg6(n)

 901   format(1x,"SoilBIO =",f7.5,1x,"SoilHUM =",f7.5,1x,
     4 "SoilCO2 =",f10.8)
      write(OutputFileUnit,120)
      write(OutputFileUnit,120)
       write(OutputFileUnit, *) "RASCHET ZA GOD RAZLOGJENIJ ORGANIKI UDOBRENIY,;"
       write(OutputFileUnit, *) "   VSEX KOMPONENTOV, t/ga ;"
      write(OutputFileUnit,120)
      write(OutputFileUnit,922)rdg7(n),rdg8(n),rdg9(n)


 922   format(1x,"FumBIO =",f7.5,1x,"FumHUM =",f7.5,1x,
     4 "FUMCO2 =",f10.8)
      write(OutputFileUnit,120)
      write(OutputFileUnit,120)
       write(OutputFileUnit, *) "RASCHET ZA GOD RAZLOGJENIJ Rastitenix ostatkov,;"
       write(OutputFileUnit, *) "organiki pochvi i organicheskogo veschestva ;"
       write(OutputFileUnit, *) "organicheskix udobreniy na pole proekta, t/ga ;"
      write(OutputFileUnit,120)
c      write(OutputFileUnit,922)rdg10(n),rdg11(n),rdg12(n)
      write(OutputFileUnit,902)rdg10(n),rdg11(n),rmg1(n)


 902   format(1x,"SumBIO pole =",f7.5,1x,"SumHUM pole =",f7.5,1x,
     4 "SSSHum =",f7.5)
      write(OutputFileUnit,120)

      write(OutputFileUnit,120)
       write(OutputFileUnit, *) "RASCHET ZA GOD VIDELENIJ METANA IZ Rastitenix 
     4 ostatkov,;"
       write(OutputFileUnit, *) "organiki pochvi i organicheskogo veschestva ;"
       write(OutputFileUnit, *) "organicheskix udobreniy na pole proekta, t/ga ;"
      write(OutputFileUnit,120)
      write(OutputFileUnit,932)rdrsCH(n),rdsoCH(n),rdfmCH(n)


 932   format(1x,"rastCH pole =",f7.5,1x,"soilCH pole =",f7.5,1x,
     4 "fumCH pole =",f7.5)
      write(OutputFileUnit,120)

      
c      write(OutputFileUnit,120)
      write(OutputFileUnit,120)
       write(OutputFileUnit, *) "VIDILENIE METANA iz  POLE  PROEKTA za god;"
       write(OutputFileUnit,837) SummarnoeWidilenieMetana*1000
 837  format(1x,"VIDILENIE  METANA  , kg CH4/ ga =",f10.5)
      write(OutputFileUnit,120)

c      write(OutputFileUnit,120)
      write(OutputFileUnit,120)
       write(OutputFileUnit, *) "VIDILENIE CO2 iz  POLE  PROEKTA za god;"
       write(OutputFileUnit,828)gn7(n)
 828  format(1x,"VIDILENIE CO2  , t C /ga =",f10.5)
      write(OutputFileUnit,120)
c      write(OutputFileUnit,120)

c      write(OutputFileUnit,120)
      write(OutputFileUnit,120)
       write(OutputFileUnit, *) "VIDILENIE DIOKSIDA UGLERODA - CO2 iz  POLE 
     4  PROEKTA za god;"
       write(OutputFileUnit,228)(gn7(n)*3.67)
 228  format(1x,"VIDILENIE DIOKSIDA UGLERODA - CO2  , t/ga =",f10.5)
      write(OutputFileUnit,120)
c      write(OutputFileUnit,120)

c      write(OutputFileUnit,120)
      write(OutputFileUnit,120)
       write(OutputFileUnit, *) "VIDILENIE N2O iz  POLE  PROEKTA za god;"
       write(OutputFileUnit,238)gn3(n)
 238  format(1x,"VIDILENIE N2O  , kg N /ga =",f10.5)
      write(OutputFileUnit,120)
c      write(OutputFileUnit,120)



       write(OutputFileUnit, *) "PRIBAVKA UGLERODA za schet Razlogenij za god;"
       write(OutputFileUnit,841)(rdg10(n)+rdg11(n)+rmg1(n))

 841  format(1x,"PRIBAVKA UGLEROD za schet Razlogenij, t/ga =",f10.5)
      write(OutputFileUnit,120)

       write(OutputFileUnit, *) "VINOS  UGLEROGA S  MASSOY  UROGJAJ za god;"
       write(OutputFileUnit,845)rmgw1(n)
 845  format(1x," VINOS  UGLEROGA S  MASSOY  UROGJAJ , t/ga =",f10.5)
      write(OutputFileUnit,120)

       write(OutputFileUnit, *) "SUMMARNOE PRIRASCHENIE  UGLEROGA za god;"
       write(OutputFileUnit, *) " s uchetom vibrosov CO2 pochvi i vinosa massoy urogaj
     4  za god;"
       write(OutputFileUnit,846)(rdg10(n)+rdg11(n)+rmg1(n)-rdg6(n)-rmgw1(n))

 846  format(1x,"SUMMARNOE PRIRASCHENIE UGLEROGA na pole,t/ga =",f10.5)
      write(OutputFileUnit,120)

       write(OutputFileUnit, *) "BALANS  UGLERODA NA POLE  PROEKTA;"
       write(OutputFileUnit,827)rdg13(n)
 827  format(1x,"BALANS UGLERODA , t/ga =",f10.5)
      write(OutputFileUnit,120)
      write(OutputFileUnit,120)

c      zapis  v maliy fail
c      malo
      write(7,120)
      write(7,9117)
 9117 format(10x,"RASCHET ZA GOD RAZLOGJENIJ RASTITELNIX OSTATKOV,;")
      write(7,9118)
 9118 format(10x,"   VSEX KOMPONENTOV, t/ga ;")
      write(7,120)
      write(7,900)rdg1(n),rdg2(n),rdg3(n)
c      malo
c      malo
      write(7,120)
      write(7,9119)
 9119 format(10x,"RASCHET ZA GOD NERAZLOGIVSHIXS RASTIT OSTATKOV,t/ga;")
      write(7,120)
      write(7,876)rmgw4(n)
c      malo
c      malo
      write(7,120)
      write(7,9120)
 9120 format(10x,"RASCHET ZA GOD RAZLOGJENIJ ORGANIKI POCHVI,;")
      write(7,9121)
 9121 format(10x,"   VSEX KOMPONENTOV, t/ga ;")
      write(7,120)
      write(7,901)rdg4(n),rdg5(n),rdg6(n)
      write(7,120)
c      malo
c      malo
      write(7,9122)
 9122 format(10x,"RASCHET ZA GOD RAZLOGJENIJ ORGANIKI UDOBRENIY,;")
      write(7,9123)
 9123 format(10x,"   VSEX KOMPONENTOV, t/ga ;")
      write(7,120)
      write(7,922)rdg7(n),rdg8(n),rdg9(n)
      write(7,120)
c      malo
c      malo
      write(7,9124)
 9124 format(10x,"RASCHET ZA GOD RAZLOGJENIJ Rastitenix ostatkov,;")
      write(7,9125)
 9125 format(10x,"organiki pochvi i organicheskogo veschestva ;")
      write(7,9126)
 9126 format(10x,"organicheskix udobreniy na pole proekta, t/ga ;")
      write(7,120)
      write(7,902)rdg10(n),rdg11(n),rmg1(n)
      write(7,120)
c      malo
c      malo
      write(7,9127)
 9127 format(10x,"VIDILENIE CO2 iz  POLE  PROEKTA za god, t/ga;")
       write(7,828)rdg12(n)
      write(7,120)
c      malo
c      malo
      write(7,9128)
 9128 format(10x,"PRIBAVKA UGLERODA za schet Razlogenij za god, t/ga;")
       write(7,841)(rdg10(n)+rdg11(n)+rmg1(n))
      write(7,120)
c      malo
c      malo
      write(7,9129)
 9129 format(10x,"VINOS  UGLEROGA S  MASSOY  UROGJAJ za god, t/ga;")
       write(7,845)rmgw1(n)
      write(7,120)
c      malo
c      malo
      write(7,9130)
 9130 format(10x,"SUMMARNOE PRIRASCHENIE  UGLEROGA za god, t/ga;")
cccccccccc       write(7,846)(rdg10(n)+rdg11(n)+rmg1(n)-rdg6(n)-rmgw1(n))
       write(7,846)(rdg10(n)+rdg11(n)+rmg1(n)-gn7(n)-rmgw1(n))

      write(7,120)
c      malo
c      malo
      write(7,9131)
 9131 format(10x,"BALANS  UGLERODA NA POLE  PROEKTA, t/ga;")
       write(7,827)rdg13(n)
      write(7,120)
c      malo

c      zapis  v ochen maliy fail
c      ochen malo
      write(8,9132)
 9132 format(10x,"SUMMARNOE PRIRASCHENIE  UGLEROGA za god, t/ga;")
       write(8,846)(rdg10(n)+rdg11(n)+rmg1(n)-rdg6(n)-rmgw1(n))
      write(8,120)
c      ochen malo
c      ochen malo
      write(8,9133)
 9133 format(10x,"BALANS  UGLERODA NA POLE  PROEKTA, t/ga;")
       write(8,827)rdg13(n)
      write(8,120)


c      write(8,9373)
c 9373 format(10x,"BALANS  UGLERODA NA POLE  PROEKTA, t/ga;")
c       write(8,827)SMNrst(1)
c      write(8,120)

c      ochen malo


c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      close (unit=OutputFileUnit)
      close (unit=7)
      close (unit=8)
      
      end

