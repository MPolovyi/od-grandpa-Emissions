      use FileReading
      use FormattedOutput
c      =========================================================================
c             M O D E L   D I N A M I K I   O R G A N I C H N O I   R E C H O V I N I 
c            V   O R G A N I C H N I X   G R U N T A X (T O R F O V I S C H A X)   T A 
c            V I K I D I V   V U G L E Z U,  M E T A N U   i   Z A K I S U   A Z O T U 
c                              Z   Z I X    G R U N T I V   (Peat-GHG-Model)    


             
c==========================================================================
      
      dimension a9(9)
      dimension llm(21),qm(21),ts1m(21),ts2m(21)
      dimension j1m(21),gim(21),tss(360),ts11(21),epot(21),filt(21)
      dimension ossrnz(21),srdww(21),osnz(21),rstek(21),dw41(21),
     4 dw51(21),CperW1(21),CperW2(21),
     5 RPM(36),DPM(36),BIO1(36),HUM1(36),IOM(36),CUrOst(36),
     6 raC(36),TSMD(36),MTSMD(36),rbC(36),rcC(36),Tpoch(36),TIsp(36),
     7 rd51(36),rd42(36),HUM2(36),BIO2(36),DBIO(36),DHUM(36),DCO2(36),
     8  R1CO2(36),R2CO2(36),ratX(36),rE(36),rMCsoi(36),rMNsoi(36),
     9 BIO3(36),HUM3(36),R3CO(36),DPM0(36),RPM0(36),SDPM(36),SRPM(36),
     5 SBIO(36),SHUM(36),SDPM0(36),SRPM0(36),SBIO0(36),SHUM0(36),
     5 RIOM(36),SDBIO(36),SDHUM(36),SDCO(36),SBIO1(36),SHUM1(36),
     5 SR1CO(36),SBIO2(36),SHUM2(36),SR2CO(36),SBIO3(36),SHUM3(36),
     5 SR3CO(36),FDPM(36),FRPM(36),FHUM(36),SHUM00(36),
     5 FDBIO(36),FDHUM(36),FDCO(36),FBIO1(36),FHUM1(36),
     5 FR1CO(36),FBIO3(36),FHUM3(36),FR3CO(36),
     5 rd1(36),rd2(36),rd3(36),rd4(36),rd5(36),rd6(36),rd7(36),rd8(36),
     5 rd9(36),rd10(36),rd11(36),rd12(36),
     5 rds1(36),rds2(36),rds3(36),rds4(36),rds5(36),rds6(36),rds7(36),
     5 rds8(36),rds9(36),rds10(36),rds11(36),rds12(36),
     5 rdf1(36),rdf2(36),rdf3(36),rdf4(36),rdf5(36),rdf6(36),rdf7(36),
     5 rdf8(36),rdf9(36),rdg1(36),rdg2(36),
     5 rdg3(36),rdg4(36),rdg5(36),rdg6(36),rdg7(36),rdg8(36),
     5 rdg9(36),rdg10(36),rdg11(36),rdg12(36),rdg13(36),rdg14(36),
     5 rdh1(36),rdh2(36),rdh3(36),rdh4(36),rmg1(36),rmg2(36),
     5 rmgw1(36),RDPM(36),RRPM(36),rmgw2(36),rmgw3(36),rmgw4(36),
     5 rmgw5(36),rchW2(36),pBIO(36),pHUM(36),pCO2(36)
	    Dimension PrRPM(36),PrDPM(36),PrBIO1(36),PrHUM1(36),
     5 PrHUM2(36),PrBIO2(36),PrDBIO(36),PrDHUM(36),PrDCO2(36),
     8  PrR1CO(36),PrR2CO(36),Prrdh1(36),Prrdh2(36),Prrdh3(36),
     8 Prrdh4(36),
     9 PrBIO3(36),PrHUM3(36),PrR3CO(36),PrDPM0(36),PrRPM0(36),
     9 BIOrst(36),BIOsoi(36),BIOfum(36),HUMrst(36),HUMsoi(36),
     9 HUMfum(36),CO2rst(36),CO2soi(36),CO2fum(36),PoleC(36),
     9 PolCO2(36),rgdmn1(36)

       dimension Whgr(21),rmW(21),rmpH(21), rmT1(21),rmW1(21),rmpH1(21),
     5 rxrd1(21),rxrd2(21),CHBIO1(21),CHHUM1(21),CHBIO2(21),CHHUM2(21),
     5 CHBIO3(21),CHHUM3(21),HSBIO(21),HHUM(21),HSBIO1(21),HHUM1(21),
     5 HSBIO2(21),HHUM2(21),HSBIO3(21),HHUM3(21),CHBIO(21),CHHUM(21),
     5 HSBIO0(21),HHUM0(21),rch1(21),rch2(21),HHUM00(21),rch3(21),
     6 rch4(21),rch5(21),rch6(21),rchW1(21),
     6 PCHBIO(36),PCHHUM(36),PCHB1(36),PCHH1(36),PCHB2(36),PCHH2(36),
     5 PCHB3(36),PCHH3(36),CHBIso(36),CHHUso(36),CHsoil(36),CHrst(36),
     5 CHBIrs(36),CHHUrs(36),FDPM0(36),FRPM0(36),FHUM0(36),CHFBI0(36),
     5 CHFHU0(36),CHFBI1(36),CHFHU1(36),CHFBI3(36),CHFHU3(36),
     5 CHBIfm(36),CHHUfm(36),CHfum(36),drww1(36),prCOrs(36)
        dimension rmt2(36),rmW2(36),rmpH2(36),SMCrst(36),rCrst(36),
     4 SMCsoi(36),SMCfum(36),rCsoil(36),rCfum(36),SMNrst(36),rNrst(36),
     4 SMNsoi(36),SMNfum(36),rNsoil(36),rNfum(36),CNrst(36),CNsoil(36),
     4 CNfum(36),CNsum(36),VNnitr(36),VNN2O(36),VNNO(36),denNO3(36),
     4 denW(36),denCO2(36),denpW(36),dnpNO3(36),VNdeni(36),VNdN2(36),
     4 VNdN2O(36),VNvfum(36),VNvfrt(36),Winfil(36),RNinfl(36),
     4 Nimmob(36),rdCHrs(36),rdCHso(36),rdCHfm(36),rdRsCH(36),
     4 RNupt(36),OtnUrs(36),rdSoCH(36),rdFmCH(36),rdPLCH(36),
     4 rOCNrs(36),rdNH4(36),rdNO3(36),rdNN20(36),BNNH4(36),BNNO3(36),
     5 rNmicr(36),FdWFPS(36),FdNO3(36),FdCO2(36),FdSCO2(36),
     4 gn1(36),gn2(36),gn3(36),gn4(36),gn5(36),gn6(36),gn7(36),
     > rmW3(36)


      real*8 sumOs,SmDBIO,SmBIO1,SmBIO2,SmBIO3,SmDHUM,SmHUM1,SmHUM2,
     4 SmHUM3,SmDCO,SmR1CO,SmR2CO,SmR3CO,SsDBIO,SsBIO1,SsBIO2,SsBIO3,
     4 SsDHUM,SsHUM1,SsHUM2,SsHUM3,SsDCO,SsR1CO,SsR2CO,SsR3CO,
     5 SFDBIO, SFBIO1,SFBIO3,SFDHUM,SFHUM1,SFHUM3,SFDCO,SFR1CO,
     5 SFR3CO,RtBIO,RtHUM,RtCO,RslBIO,RslHUM,RslCO,RFBIO,RFHUM,
     5 RFCO, SumBIO,SumHUM,SumCO,BalanC,RIOM,SSSHUM,
     4 PerzW1,PerzW2,NorW1,NorW2,RRSHUM,UR,SumRst,prSMCO,
     5 RKWin,PKpoch,PKcult,RWin,SumDPM,SumRPM,RazDPM,RazRPM,
     3 TSMCrs,TSMCso,TSMCfm,RNNH4,RNNO3,TSMNrs,TSMNso,TSMNfm,TMNN2O,
     4 SMCHrs,SMCHso,SMCHfm,rastCH,soilCH,fumCH,SMCHPL,OCNrst,
     4 SnN2O,SdN2O,SN2O,SMCOrs,SMCOso,SMCOfm,SPolCO
      
      integer j

      integer gi,g2,gim,rd41,b1,b2,b3

      integer, parameter :: OutputFileUnit = 106

      integer :: n, t0, n1, n2
      real :: fi
      integer, dimension(:), allocatable :: dv
      real, dimension(:), allocatable :: ts, os, dww, usl2, usl3, usl4, inf, rnitr, hgr

      BalanC=0

c      open (unit=InputFileUnit, file='Peat-mod-3.dat',status='old',form='formatted')
      Open (UNIT=OutputFileUnit,FILE='Peat-mod-main-3m7.res')
      Open (UNIT=7,FILE='Peat-mod-3m7.res')
      Open (UNIT=8,FILE='Peat-mod-omm7.res')



c      read(InputFileUnit, "(4a4)") a55
c      read(InputFileUnit, "(4a4)") a1,a2,a3,a4
c      read(InputFileUnit, "(4a20)")
c      read(InputFileUnit, "(4a20)")
c      read(InputFileUnit, "(4i3,f6.2)")n,t0,n1,n2,fi
c      read(InputFileUnit, "(4a20)")
c      read(InputFileUnit, "(14f5.1)")  (ts(j),j=1,n)
c      read(InputFileUnit, "(4a20)")
c      read(InputFileUnit, "(14f5.1)")  (os(j),j=1,n)
c      read(InputFileUnit, "(4a20)")
c      read(InputFileUnit, "(14f5.1)")  (dww(j),j=1,n)
c      read(InputFileUnit, "(4a20)")
c      read(InputFileUnit, "(14f5.1)")  (usl2(j),j=1,n)
c      read(InputFileUnit, "(4a20)")
c      read(InputFileUnit, "(4a20)")
c      read(InputFileUnit, "(14f5.1)")  (usl3(j),j=1,n)
c      read(InputFileUnit, "(4a20)")
c
c      read(InputFileUnit, "(14f5.1)")  (usl4(j),j=1,n)
c      read(InputFileUnit, "(4a20)")
c      read(InputFileUnit, "(24i3)")    (dv(j),j=1,n)
c      read(InputFileUnit, "(4a20)")
c      read(InputFileUnit, "(9f8.3)")   (inf(j),j=1,21)
c      read(InputFileUnit, "(4a20)")
c      read(InputFileUnit, "(9f8.3)")   (rnitr(j),j=1,15)
c      read(InputFileUnit, "(4a20)")
c      read(InputFileUnit, "(12f7.1)")  (hgr(j),j=1,n)

      call ReadDataFromFile('Peat-mod-3.dat', Int1=n, Int2=t0, Int3=n1, Int4=n2, 
     >      Real1=fi, IntArrOne1=dv, 
     >      RealArrOne1=ts, RealArrOne2=os, RealArrOne3=dww, RealArrOne4=usl2, RealArrOne5=usl3,
     >      RealArrOne6=usl4, RealArrOne7=inf, RealArrOne8=rnitr, RealArrOne9=hgr)

      if(i.ne.1)inf(8)=BalanC
      if(i.ne.1)inf(1)=SumDPM

 4118 format(4x,76('*'))
 4117 format(10x,'MODEL  DINAMIKI  ORGANICHNOI  RECHOVINI') 
 4120 format(10x,'V  ORGANICHNIX  GRUNTAX(TORFOVISCHAX)  TA') 
 4813 format(10x,'VIKIDIV  VUGLEZU, METANU  i  ZAKISU  AZOTU') 
 4119 format(10x,'        Z  ZIX   GRUNTIV  (Peat-GHG-Model)  ')    




c 4117 format(10x,'M O D E L  N A K O P L E N I J  U G L E R O D A')
c 4120 format(10x,'       V  P O C H V E   ')
c 4813 format(10x,' MODIFIZIROVANNIY VARIANT ROTHC-26.3  modeli     ') 
c 4119 format(4x,76('*'))
 4193 format(4x,76('*'))
  117 format(10x,'      W X O D N A J   I N F O R M A Z I J ')
  118 format(1x,76('-'))
  119 format(1x,76('*'))
  120 format(4x,76('-'))
  121 format(' ')
  129 format(1x,76('-'))
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
      write(OutputFileUnit, *)' Srednjj za mesjz tempsratura vozduxa (grad. C):'
      write(OutputFileUnit,102) (ts(j),j=1,n)
      write(OutputFileUnit, *) '      Summa osadkov za mesjz (mm):'
      write(OutputFileUnit,102) (os(j),j=1,n)
      write(OutputFileUnit, *) ' Srednjj za mesjz otnositelnaj vlagnost vozduxa (%):'
      write(OutputFileUnit,102) (dww(j),j=1,n)
      write(OutputFileUnit, *) ' Uslovnaj velichina 2 (nomer mesjza goda):'
      write(OutputFileUnit,102) (usl2(j),j=1,n)
      write(OutputFileUnit, *) 'Chislo dekad v mesjze, kogda uroven gruntovix vod)'
      write(OutputFileUnit, *) ' raven i menee 20 sm)'

      write(OutputFileUnit,102) (usl3(j),j=1,n)

      write(OutputFileUnit, *) ' Uslovnaj velichina(mesjzi vegetazii kulturi):'
      write(OutputFileUnit,102) (usl4(j),j=1,n)
      write(OutputFileUnit, *) '     Chislo dney v raschetnom mesjze :'
      write(OutputFileUnit,115) (dv(j),j=1,n)
      write(OutputFileUnit,118)
      write(OutputFileUnit, *) '  M A S S I V  " I N F " - parametri modeli   :'
      write(OutputFileUnit,101)(inf(j),j=1,21)
      write(OutputFileUnit, *) '  M A S S I V  " Rnitr " - parametri modeli   :'
      write(OutputFileUnit,101)(rnitr(j),j=1,15)
      write(OutputFileUnit, *) ' Sredniy za mesjz uroven gruntovix vod (sm):'
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
c       write(OutputFileUnit, *) 'inf(10) - kod prirodnoy rastitelnosti:
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

c     write(OutputFileUnit, *) 'inf(11) - massa nadzemnoy chasti prirodnoy rastitelnosti,  t/ga'
c       write(OutputFileUnit, *) 'inf(12) - dolj otmershey nadzemnoy chasti (trav ili listev dereva),  otn. ed.'
c       write(OutputFileUnit, *) 'inf(13) - otnoshenie nadzemnoy k podzemnoy chasti rasteniy ,  otn.ed.'
c       write(OutputFileUnit, *) 'inf(14) - dolj otmershix korney,  otn.ed.'
c       write(OutputFileUnit, *) 'inf(15) - wlgjnost zavjdanij,  mm'
c        write(OutputFileUnit, *) 'inf(16) - naimenshaj wlagoemkost,  mm'
c        write(OutputFileUnit, *) 'inf(17) - wlagoemkost nasishenij,  mm'
c        write(OutputFileUnit, *) 'inf(18) - pH pochvi,  otn.ed.'
c        write(OutputFileUnit, *) 'inf(19) - objemnaj massa pochvi,  g/cm3.'
c        write(OutputFileUnit, *) 'inf(20) - organicheskoe veschestvo pochvi (C), %
c        write(OutputFileUnit, *) 'inf(21) - kolichestvo kustov verboloza ili kolichestvo derevev (verbi) na 1 ga, shtuk /ga
c==========================================================================
c             O P I S A N I E    M A S S I V A     " Rnitr "  
c==========================================================================

c        write(OutputFileUnit, *) 'rnitr(1) - soderganie ammonij NH4 v pochvi, mg N / 100 g pochvi 
c        write(OutputFileUnit, *) 'rnitr(2) - soderganie nitratov NO3 v pochvi, mg N / 100 g pochvi
c        write(OutputFileUnit, *) 'rnitr(3) - otnoshenie C/N dlj rastitelnix ostatkov, otn.ed. 
c        write(OutputFileUnit, *) 'rnitr(4) - otnoshenie C/N dlj pochvi, otn.ed.
c        write(OutputFileUnit, *) 'rnitr(5) - otnoshenie C/N dlj udobreniy, otn.ed. 
c        write(OutputFileUnit, *) 'rnitr(6) - otnoshenie C/N  summarnoe - obobschennoe, otn.ed.
c       write(OutputFileUnit, *) 'rnitr(7) - kolichestvo vnesennogo azotnogo udobrenij,kg N / ga
c        write(OutputFileUnit, *) 'rnitr(8) - dolj N v azotnom udobrenii, otn. od.
c        write(OutputFileUnit, *) 'rnitr(9) - kriticheskoe kolichestvo osadkov, pri kotorom 
c                             nachinaetsj vivetrivanie ammonij
c       write(OutputFileUnit, *) 'rnitr(10) - dolj ammoniju v pogloschennom rasteniem  azote
c       write(OutputFileUnit, *) 'rnitr(11) - dolj nitraotv  v pogloschennom rasteniem  azote
c===========================================================================
c  NACHALNIe DANNIe KOMPONENTOV ORGANICHNOGO MATERIALA   POCHVI
c================================================================
c        SDPM(j)=0.004527*inf(8)
c        SRPM(j)=0.1324*inf(8)
c        SBIO(j)=0.0197*inf(8)
c        SHUM(j)=0.7636*inf(8)

c       write(OutputFileUnit, *) 'rnitr(12) -OMSDPM(j) 
c       write(OutputFileUnit, *) 'rnitr(13) -OMSRPM(j) 
c       write(OutputFileUnit, *) 'rnitr(14) -OMSBIO(j) 
c       write(OutputFileUnit, *) 'rnitr(15) -OMSHUM(j) 
c==========================================================================
      write(OutputFileUnit,129)
      write(OutputFileUnit,119)
      write(OutputFileUnit,122)
  122 format(10x,'     R E S U L T A T     R A S C H E T O V       ')
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
      SMCHPL=0  
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

         if(os(j) > TIsp(j)) then
            TSMD(j) = 0
         end if

         if(TSMD(j) < rd51(j)) then
            TSMD(j) = rd51(j)
         end if
         
         rd41=rd41+tsmd(j)
c  rd41 - AccTSMD
         if(os(j) > TIsp(j)) then
            rd41 = 0
         end if

         if(rd41 < rd51(j)) then
            rd41 = rd51(j)+0.01
         end if
c=============================================================
c===============================================+++++++++++
         Whgr(j) = -1.5643*(-1*hgr(j))+452.32
     
         if((Whgr(j)-inf(15)) < (inf(16)-inf(15))/2) then
            rmW(j)= 0.2+(0.8*2/(inf(16)-Inf(15)))*(Whgr(j)-inf(15))
         end if

     
         if((Whgr(j)-inf(15)) > (inf(16)-inf(15))/2 .and. (Whgr(j)-inf(15)) < (inf(16)-inf(15))) then
            rmW(j)=1.0
         end if


         if((Whgr(j)-inf(15)) > (inf(16)-inf(15))) then
            rmW(j)= 1-(0.8/((inf(17)-inf(15))-(inf(16)-inf(15))))*((Whgr(j)-inf(15))-(inf(16)-inf(15)))
         end if
     
c        if(Whgr(j).gt.inf(16)) rmW(j)= 0.8-(0.8/(inf(17)-inf(16)))*
c     6   (Whgr(j)-(inf(16)))

ccccccccccccccc	rchW1(j)=(Whgr(j)-inf(15))/(inf(16)-inf(15))

         rchW1(j)=Whgr(j)/inf(16)

C       if(Whgr(j).gt.(inf(16))) rmW1(j)=381.2*rchW1(j)**4-
C    6 1643.3*rchW1(j)**3+2658.9*rchW1(j)**2-1913*rchW1(j)+
C    6 516.21


        if(Whgr(j).lt.(inf(16))) rmW1(j)=0
        if((Whgr(j).gt.(inf(16))) .and. (Whgr(j).lt.(inf(17))))
     >   rmW1(j)=1-(381.2*rchW1(j)**4-1643.3*rchW1(j)**3+
     >   2658.9*rchW1(j)**2-1913*rchW1(j)+516.21)
        if(Whgr(j).gt.(inf(17))) rmW1(j)=1
ccc       if(rmW1(j).gt.1) rmW1(j)=1
cc     6   -inf(15))))/(inf(17)-(inf(16)-inf(15)))


         rmpH(j)=0.2+(1-0.2)*((inf(18)-2)/(5-2))

         if(Whgr(j) > inf(16)) then
            rmpH(j)=((1.0**(1/(-50))+exp((-1)*inf(18))))**(-50) 
         end if
c==================================================
ccccccccc       rmT1(j)=47.9/(1+exp(106/(Tpoch(j)+18.3)))
c       rmW3(j)=0.5
         rmpH1(j)=((1.0**(1/(-50))+exp((-1)*inf(18))))**(-50)

         rmW3(j) = -0.0012*(hgr(j)**2) + 0.002*hgr(j) + 1.006

C         Whgr(5) = -1.5643*(-1*20)+452.32


c============================================================
c  RASCHET TEMPERATURI POCHVI NA GLUBINE 20 cm
c=============================================================
c - Tpoch - temperatura pochvi na glubine 20 cm
         if(usl2(j).eq.1)  Tpoch(j)=ts(j)+2.7
         if(usl2(j).eq.2)  Tpoch(j)=ts(j)+0.74
         if(usl2(j).eq.3)  Tpoch(j)=ts(j)-0.6
         if(usl2(j).eq.4)  Tpoch(j)=ts(j)+1.9
         if(usl2(j).eq.5)  Tpoch(j)=ts(j)+4.4
         if(usl2(j).eq.6)  Tpoch(j)=ts(j)+4.2
         if(usl2(j).eq.7)  Tpoch(j)=ts(j)+3.1
         if(usl2(j).eq.8)  Tpoch(j)=ts(j)+2.8
         if(usl2(j).eq.9)  Tpoch(j)=ts(j)+2.1
         if(usl2(j).eq.10) Tpoch(j)=ts(j)+2.2
         if(usl2(j).eq.11) Tpoch(j)=ts(j)+4.5
         if(usl2(j).eq.12) Tpoch(j)=ts(j)+3.1



c=================================================================
c       RASCHET  KOEFFIZIENTOV  BAZOVOGO I  VSPOMAGATELNIX URAVNENIY
c  dlj rascheta razlogenij organicheskogo materiala pochvi
c==================================================================
c         raC(j)=47.9/(1+exp(106/(Tpoch(j)+18.3)))
         raC(j)=47.9/(1+exp(125/(Tpoch(j)+18.3)))
         rmt1(j)=47.9/(1+exp(125/(Tpoch(j)+18.3)))

         if(Tpoch(j).lt.0) raC(j)=0
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
c      if(inf(10).eq.1)SumRst=inf(12)*inf(11)+(1-inf(13))*inf(11)*inf(14) 
c      if(inf(10).eq.2)SumRst=inf(12)*inf(11)+(1-inf(13))*inf(11)*inf(14)
c      if(inf(10).eq.3)SumRst=inf(12)*inf(11)+(1-inf(13))*inf(11)*inf(14)
c      if(inf(10).eq.4)SumRst=inf(12)*inf(11)+(1-inf(13))*inf(11)*inf(14)
c      if(inf(10).eq.5)SumRst=inf(12)*inf(11)+(1-inf(13))*inf(11)*inf(14)
c      if(inf(10).eq.6)SumRst=inf(12)*inf(11)+(1-inf(13))*inf(11)*inf(14)


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
c          'inf(7) - nomer pervogo mesjza vegetazii kulturi'

         CUrost(j)=((2.3026*(2./3)*10.**(2.-(2./3)*(j-inf(7)+1)))/(1.+10.**(2.-(2./3)*(j-inf(7)+1)))**2)*SumRst

         if(j.lt.inf(7)) CUrost(j)=0
         if(j.gt.(inf(7)+inf(5))) CUrost(j)=0

c================================================================
c      RASCHET  PERVICHNOGO RAZLOGENIJ RASTITELNIX OSTATKOV
c================================================================
c       DPM0(j)=0.59*CUrost(j)/30
c       RPM0(j)=0.41*CUrost(j)/30

         DPM0(j)=0.59*CUrost(j)
         RPM0(j)=0.41*CUrost(j)



         DPM(j)=DPM0(j)*(1-exp(-rmt1(j)*rmW(j)*rmpH(j)*0.6*10.0*0.00278))
         RPM(j)=RPM0(j)*(1-exp(-rmt1(j)*rmW(j)*rmpH(j)*0.6*0.3*0.00278))

cccc   *0.03333
c================================================================
c      Razlogenie DPM na DBIO, DHUM,  DCO2
c================================================================
         DBIO(j)=DPM(j)*(1/(1+ratX(j)))*0.46
         DHUM(j)=DPM(j)*(1/(1+ratX(j)))*0.54

c        DBIO(j)=DPM(j)*(1-((1/ratX(j))-(0.85/(1+(1/ratX(j)))))
c     5 -(0.85/(1
c     5+(1/ratX(j)))))*0.46
c          DHUM(j)=DPM(j)*(0.85/(1+(1/ratX(j))))*0.54

cc         *(1-(1/(1+ratX(j)))*0.46-(1/(1+ratX(j)))*0.54)

         DCO2(j)=DPM(j)-(DBIO(j)+DHUM(j))

c        DCO2(j)=DPM(j)*(1-((1/ratX(j))))


c       CHBIO(j)=(1-((1/ratX(j))-(0.85/(1+(1/ratX(j)))))-(0.85/(1
c     5+(1/ratX(j)))))
c     6 *(DBIO(j)*rmW3(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.66*0.00278))
c     6 )
        
         if(hgr(j) < -20)CHBIO(j)=0


c       CHHUM(j)=(1-((1/ratX(j))-(0.85/(1+(1/ratX(j)))))-(0.85/(1
c     5+(1/ratX(j)))))
c     6 *(DHUM(j)*rmW3(j)*(exp(-rmt1(j)*rmpH1(j)
c     6 *0.6*0.02*0.00278)))
         if(hgr(j) < -20)CHHUM(j)=0

         CHBIO(j)=DBIO(j)*rmW3(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.66*0.00278))*(1-(0.85/(1+rE(j))))
         if(hgr(j) < -20)CHBIO(j)=0


         CHHUM(j)=DHUM(j)*rmW3(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.02*0.00278))*(1-0.85*(0.85/(1+rE(j))))
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


         CHBIO1(j)=BIO1(j)*rmW3(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.66*0.00278))*(1-(0.85/(1+rE(j))))
         if(hgr(j) < -20)CHBIO1(j)=0

         CHHUM1(j)=HUM1(j)*rmW3(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.02*0.00278))*(1-0.85*(0.85/(1+rE(j))))
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
c       rdh1(j)=(DPM(j)*rmW3(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.66*0.00278)
c     6  )
c     6+DBIO(j))*(1-exp(-rmt1(j)*rmW3(j)*rmpH1(j)*0.6*0.66*0.00278))

         rdh1(j)=BIO1(j)*(1-exp(-rmt1(j)*rmW(j)*rmpH(j)*0.6*0.66*0.00278))

         BIO2(j)=rdh1(j)*(1/(1+ratX(j)))*0.46

c       rdh2(j)=(DPM(j)*rmW3(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.02*0.08333)
c     6  )
c     6+DHUM(j))*(1-exp(-rmt1(j)*rmW3(j)*rmpH1(j)*0.6*0.02*0.08333))

cc       rdh2(j)=DHUM(j))*(1-exp(-rmt1(j)*rmW3(j)*rmpH1(j)*0.6*0.02*0.08333))

         rdh2(j)=BIO1(j)*(1-exp(-rmt1(j)*rmW(j)*rmpH(j)*0.6*0.02*0.00278))


     
         HUM2(j)=rdh2(j)*(1/(1+ratX(j)))*0.54

         R2CO2(j)=BIO1(j)-(BIO2(j)+HUM2(j))


c      CHBIO2(j)=(1-((1/ratX(j))-(0.85/(1+(1/ratX(j)))))-(0.85/(1
c     5+(1/ratX(j)))))
c     6 *(BIO2(j)*rmW3(j)*(exp(-rmt1(j)
c     6 *rmpH1(j)*0.6*0.66*0.08333)))
         if(hgr(j) < -20)CHBIO2(j)=0

c      CHHUM2(j)=(1-((1/ratX(j))-(0.85/(1+(1/ratX(j)))))-(0.85/(1
c     5+(1/ratX(j)))))
c     6 *(HUM2(j)*rmW3(j)*(exp(-rmt1(j)*rmpH1(j)
c     6 *0.6*0.02*0.08333)))
         if(hgr(j) < -20)CHHUM2(j)=0

         CHBIO2(j)=BIO2(j)*rmW3(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.66*0.00278))*(1-(0.85/(1+rE(j))))
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

c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         CHBIO3(j)=BIO3(j)*rmW3(j)*(exp(-raC(j)*rmpH(j)*0.6*0.66*0.00278))*(1-(0.85/(1+rE(j))))
         if(hgr(j) < -20)CHBIO3(j)=0

         CHHUM3(j)=HUM3(j)*rmW3(j)*(exp(-raC(j)*rmpH(j)*0.6*0.02*0.00278))*(1-0.85*(0.85/(1+rE(j))))
         if(hgr(j) < -20)CHHUM3(j)=0

c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

c================================================================
c      RASCHET  PERVICHNOGO RAZLOGENIJ NERAZLOGIVSCHIXSJ RASTITELNIX OSTATKOV
c            PREDSCHESTVJUSCHEGO  GODA
c================================================================


         PrDPM0(j)=0.59*inf(1)
         PrRPM0(j)=0.41*inf(1)
c       PrDPM(j)=PrDPM0(j)*((1-exp(-rmt1(j)*rmW3(j)*rmpH1(j)*0.6*10.0
c     6*0.00278)))*0.05
c       PrRPM(j)=PrRPM0(j)*((1-exp(-rmt1(j)*rmW3(j)*rmpH1(j)*0.6*0.3
c     6*0.00278)))*0.05



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
c     6 *(PrDBIO(j)*rmW3(j)*(exp(-rmt1(j)
c     6 *rmpH1(j)*0.6*0.66*0.08333)))
         if(hgr(j) < -20)PCHBIO(j)=0

c      PCHHUM(j)=(1-((1/ratX(j))-(0.85/(1+(1/ratX(j)))))-(0.85/(1
c     5+(1/ratX(j)))))
c     6 *(PrDHUM(j)*rmW3(j)*(exp(-rmt1(j)*rmpH1(j)
c     6 *0.6*0.02*0.08333)))
         if(hgr(j) < -20)PCHHUM(j)=0

         PCHBIO(j)=PrDBIO(j)*rmW3(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.66*0.00278))*(1-(0.85/(1+rE(j))))

         if(hgr(j) < -20)PCHBIO(j)=0

         PCHHUM(j)=PrDHUM(j)*rmW3(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.02*0.00278))*(1-0.85*(0.85/(1+rE(j))))

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
c     6 *(PrBIO1(j)*rmW3(j)*(exp(-rmt1(j)
c     6 *rmpH1(j)*0.6*0.66*0.08333)))
         if(hgr(j) < -20)PCHB1(j)=0

c       PCHH1(j)=(1-((1/ratX(j))-(0.85/(1+(1/ratX(j)))))-(0.85/(1
c     5+(1/ratX(j)))))
c     6 *(PrHUM1(j)*rmW3(j)*(exp(-rmt1(j)*rmpH1(j)
c     6 *0.6*0.02*0.08333)))
         if(hgr(j) < -20)PCHH1(j)=0

         PCHB1(j)=PrBIO1(j)*rmW3(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.66*0.00278))*(1-(0.85/(1+rE(j))))
         if(hgr(j) < -20) PCHB1(j)=0

         PCHH1(j)= PrHUM1(j)*rmW3(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.02*0.00278))*(1-0.85*(0.85/(1+rE(j))))

         if(hgr(j) < -20) PCHH1(j)=0

c================================================================
c      Razlogenie PrDBIO na  PrBIO2,  PrHUM2, PrR2CO22
c===============================================================
c      Prrdh1(j)- kolichestvo  BIO
c       Prrdh1(j)=(PrDPM(j)*(exp(-raC(j)*rbC(j)*0.6*0.66*0.08333))
c     6 +PrDBIO(j))*(1-exp(-raC(j)*rbC(j)*0.6*0.66*0.08333))

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
c     6 *(PrBIO2(j)*rmW3(j)*(exp(-rmt1(j)
c     6 *rmpH1(j)*0.6*0.66*0.08333)))
         if(hgr(j) < -20)PCHB2(j)=0

c       PCHH2(j)=(1-((1/ratX(j))-(0.85/(1+(1/ratX(j)))))-(0.85/(1
c     5+(1/ratX(j)))))
c     6 *(PrHUM2(j)*rmW3(j)*(exp(-rmt1(j)*rmpH1(j)
c     6 *0.6*0.02*0.08333)))
         if(hgr(j) < -20)PCHH2(j)=0

         PCHB2(j)=PrBIO2(j)*rmW3(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.66*0.00278))*(1-(0.85/(1+rE(j))))
         if(hgr(j) < -20)PCHB2(j)=0

         PCHH2(j)=PrHUM2(j)*rmW3(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.02*0.00278))*(1-0.85*(0.85/(1+rE(j))))

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

         PCHB3(j)=PrBIO3(j)*rmW3(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.66*0.00278))*(1-(0.85/(1+rE(j))))
         if(hgr(j) < -20)PCHB3(j)=0

         PCHH3(j)=PrHUM3(j)*rmW3(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.02*0.00278))*(1-0.85*(0.85/(1+rE(j))))

         if(hgr(j) < -20)PCHH3(j)=0
c====================================================================
c    Summarnoe razlogenie po vsem komponentam  rastitelnix ostatkov
c     i skorosti mineralizazii
c====================================================================
	      BIOrst(j)=(DBIO(j)+PrDBIO(j)+BIO1(j)+PrBIO1(j)+BIO2(j)+PrBIO2(j)+BIO3(j)+PrBIO3(j))

	      HUMrst(j)=(DHUM(j)+PrDHUM(j)+HUM1(j)+PrHUM1(j)+HUM2(j)+PrHUM2(j)+HUM3(j)+PrHUM3(j))
	
      	SMCrst(j)=(BIOrst(j)+HUMrst(j))*30

cccc        SMNrst(j)=SMCrst(j)/rnitr(3)*1000*30
         SMNrst(j)=SMCrst(j)*1000/rnitr(3)
cccccccccccccccc        if(j.gt.1) SMNrst(j)=SMCrst(j)/CNrst(j)  

c	CO2rst(j)=(DCO2(j)+PrDCO2(j)+R1CO2(j)+PrR1CO(j)+R2CO2(j)
c     6   +PrR2CO(j)+R3CO(j)+PrR3CO(j))*30

ccccccccccccc	CO2rst(j)=(DCO2(j)+R1CO2(j)+R2CO2(j)
cccccccccccccccccc     6   +R3CO(j))*30

	      if(usl3(j) == 0) CO2rst(j)=(DCO2(j)+R1CO2(j)+R2CO2(j)+R3CO(j))*30

	      If(usl3(j) == 1) CO2rst(j)=((DCO2(j)+R1CO2(j)+R2CO2(j)+R3CO(j))*30)*0.5*0.8

	      If(usl3(j) == 2) CO2rst(j)=((DCO2(j)+R1CO2(j)+R2CO2(j)+R3CO(j))*30)*0.5*0.6

	      If(usl3(j) == 3) CO2rst(j)=((DCO2(j)+R1CO2(j)+R2CO2(j)+R3CO(j))*30)*0.5*0.4

	      SMCOrs=SMCOrs+CO2rst(j)	


	      If(usl3(j) == 0) prCOrs(j)=(PrDCO2(j)+PrR1CO(j)+R2CO2(j)+PrR2CO(j)+PrR3CO(j))*30
	      If(usl3(j) == 1) prCOrs(j)=((PrDCO2(j)+PrR1CO(j)+R2CO2(j)+PrR2CO(j)+PrR3CO(j))*30)*0.5*0.8
	      If(usl3(j) == 2) prCOrs(j)=((PrDCO2(j)+PrR1CO(j)+R2CO2(j)+PrR2CO(j)+PrR3CO(j))*30)*0.5*0.6
	      If(usl3(j) == 3) prCOrs(j)=((PrDCO2(j)+PrR1CO(j)+R2CO2(j)+PrR2CO(j)+PrR3CO(j))*30)*0.5*0.4

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
	      If(usl3(j) == 0) CHBIrs(j)=0 
         If(usl3(j) == 1) CHBIrs(j)=(CHBIO(j)+CHBIO1(j)+CHBIO2(j)+CHBIO3(j)+PCHBIO(j)+PCHB1(j)+PCHB2(j)+PCHB3(j))*30*0.33

         If(usl3(j) == 2) CHBIrs(j)=(CHBIO(j)+CHBIO1(j)+CHBIO2(j)+CHBIO3(j)+PCHBIO(j)+PCHB1(j)+PCHB2(j)+PCHB3(j))*30*0.66

         If(usl3(j) == 3) CHBIrs(j)=(CHBIO(j)+CHBIO1(j)+CHBIO2(j)+CHBIO3(j)+PCHBIO(j)+PCHB1(j)+PCHB2(j)+PCHB3(j))*30*1.00
     

	      If(usl3(j) == 0) CHHUrs(j)=0
	      If(usl3(j) == 1) CHHUrs(j)=(CHHUM(j)+CHHUM1(j)+CHHUM2(j)+CHHUM3(j)+PCHHUM(j)+PCHH1(j)+PCHH2(j)+PCHH3(j))*30*0.33

	      If(usl3(j) == 2) CHHUrs(j)=(CHHUM(j)+CHHUM1(j)+CHHUM2(j)+CHHUM3(j)+PCHHUM(j)+PCHH1(j)+PCHH2(j)+PCHH3(j))*30*0.66

	      If(usl3(j) == 3) CHHUrs(j)=(CHHUM(j)+CHHUM1(j)+CHHUM2(j)+CHHUM3(j)+PCHHUM(j)+PCHH1(j)+PCHH2(j)+PCHH3(j))*30*1.00



     
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
c       write(OutputFileUnit, *) 'rnitr(12) -OMSDPM(j) 
c       write(OutputFileUnit, *) 'rnitr(13) -OMSRPM(j) 
c       write(OutputFileUnit, *) 'rnitr(14) -OMSBIO(j) 
c       write(OutputFileUnit, *) 'rnitr(15) -OMSHUM(j) 


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
c     6 *(SBIO0(j)*rmW3(j)*(exp(-rmt1(j)
c     6 *rmpH1(j)*0.6*0.66*0.08333)))
         if(hgr(j) < -20) HSBIO0(j)=0

c      HHUM00(j)=(1-((1/ratX(j))-(0.85/(1+(1/ratX(j)))))-(0.85/(1
c     5+(1/ratX(j)))))
c     6 *(SHUM0(j)*rmW3(j)*(exp(-rmt1(j)*rmpH1(j)
c     6 *0.6*0.02*0.08333)))
         if(hgr(j) < -20) HHUM00(j)=0


         HSBIO0(j)=SBIO0(j)*rmW3(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.66*0.00278))*(1-(0.85/(1+rE(j))))
         if(hgr(j) < -20) HSBIO0(j)=0

         HHUM00(j)=SHUM0(j)*rmW3(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.02*0.00278))*(1-0.85*(0.85/(1+rE(j))))

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
c     6 *(SDBIO(j)*rmW3(j)*(exp(-rmt1(j)
c     6 *rmpH1(j)*0.6*0.66*0.08333)))
         if(hgr(j) < -20) HSBIO(j)=0

c       HHUM0(j)=(1-((1/ratX(j))-(0.85/(1+(1/ratX(j)))))-(0.85/(1
c     5+(1/ratX(j)))))
c     6 *(SDHUM(j)*rmW3(j)*(exp(-rmt1(j)*rmpH1(j)
c     6 *0.6*0.02*0.08333)))
         if(hgr(j) < -20) HHUM0(j)=0

         HSBIO(j)=SDBIO(j)*rmW3(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.66*0.00278))*(1-(0.85/(1+rE(j))))
         if(hgr(j) < -20) HSBIO(j)=0

         HHUM0(j)=SDHUM(j)*rmW3(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.02*0.00278))*(1-0.85*(0.85/(1+rE(j))))

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
c     6 *(SBIO1(j)*rmW3(j)*(exp(-rmt1(j)
c     6 *rmpH1(j)*0.6*0.66*0.08333)))
         if(hgr(j) < -20)HSBIO1(j)=0

c       HHUM1(j)=(1-((1/ratX(j))-(0.85/(1+(1/ratX(j)))))-(0.85/(1
c     5+(1/ratX(j)))))
c     6 *(SHUM1(j)*rmW3(j)*(exp(-rmt1(j)*rmpH1(j)
c     6 *0.6*0.02*0.08333)))
         if(hgr(j) < -20)HHUM1(j)=0

         HSBIO1(j)=SBIO1(j)*rmW3(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.66*0.00278))*(1-(0.85/(1+rE(j))))
         if(hgr(j) < -20)HSBIO1(j)=0

         HHUM1(j)=SHUM1(j)*rmW3(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.02*0.00278))*(1-0.85*(0.85/(1+rE(j))))

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
c     6 *(SBIO2(j)*rmW3(j)*(exp(-rmt1(j)
c     6 *rmpH1(j)*0.6*0.66*0.08333)))
         if(hgr(j) < -20)HSBIO2(j)=0

c       HHUM2(j)=(1-((1/ratX(j))-(0.85/(1+(1/ratX(j)))))-(0.85/(1
c     5+(1/ratX(j)))))
c     6 *(SHUM2(j)*rmW3(j)*(exp(-rmt1(j)*rmpH1(j)
c     6 *0.6*0.02*0.08333)))
         if(hgr(j) < -20)HHUM2(j)=0

         HSBIO2(j)=SBIO2(j)*rmW3(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.66*0.00278))*(1-(0.85/(1+rE(j))))
         if(hgr(j) < -20)HSBIO2(j)=0

         HHUM2(j)=SHUM2(j)*rmW3(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.02*0.00278))*(1-0.85*(0.85/(1+rE(j))))

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
c     6 *(SBIO3(j)*rmW3(j)*(exp(-rmt1(j)
c     6 *rmpH1(j)*0.6*0.66*0.08333)))
         if(hgr(j) < -20)HSBIO3(j)=0

c       HHUM3(j)=(1-((1/ratX(j))-(0.85/(1+(1/ratX(j)))))-(0.85/(1
c     5+(1/ratX(j)))))
c     6 *(SHUM3(j)*rmW3(j)*(exp(-rmt1(j)*rmpH1(j)
c     6 *0.6*0.02*0.08333)))
         if(hgr(j) < -20)HHUM3(j)=0

         HSBIO3(j)=SBIO3(j)*rmW3(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.66*0.00278))*(1-(0.85/(1+rE(j))))
         if(hgr(j) < -20)HSBIO3(j)=0

         HHUM3(j)=SHUM3(j)*rmW3(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.02*0.00278))*(1-0.85*(0.85/(1+rE(j))))

         if(hgr(j) < -20)HHUM3(j)=0


c===============================================================
c    Summarnoe razlogenie po vsem komponentam pochvi
c===============================================================
         BIOsoi(j)=(SDBIO(j)+SBIO1(j)+SBIO2(j)+SBIO3(j))

         HUMsoi(j)=(SDHUM(j)+SHUM1(j)+SHUM2(j)+SHUM3(j))

	      SMCsoi(j)=(BIOsoi(j)+HUMsoi(j))*30

         SMNsoi(j)=SMCsoi(j)*1000/(rnitr(4))
c perevod v kg N/ga umnogjenie na 1000
ccccccccc        if(j.gt.1) SMNsoi(j)=SMCsoi(j)/CNsoil(j)  


         If(usl3(j).eq.0)  CO2soi(j)=(SDCO(j)+sR1CO(j)+sR2CO(j)+sR3CO(j))*30

         If(usl3(j).eq.1)  CO2soi(j)=((SDCO(j)+sR1CO(j)+sR2CO(j)+sR3CO(j))*30)*0.5*0.8

         If(usl3(j).eq.2) CO2soi(j)=((SDCO(j)+sR1CO(j)+sR2CO(j)+sR3CO(j))*30)*0.5*0.6

         If(usl3(j).eq.3) CO2soi(j)=((SDCO(j)+sR1CO(j)+sR2CO(j)+sR3CO(j))*30)*0.5*0.4

         SMCOso=SMCOso+CO2soi(j) 




c	CHBIso(j)=HSBIO0(j)*usl3(j)*30+HSBIO(j)*usl3(j)*30+HSBIO1(j)
c     4 *usl3(j)*30+HSBIO2(j)*usl3(j)*30+HSBIO3(j)*usl3(j)*30
c
c        CHHUso(j)=HHUM00(j)*usl3(j)*30+HHUM0(j)*usl3(j)*30+HHUM1(j)*
c     4 usl3(j)*30+HHUM2(j)*usl3(j)*30+HHUM3(j)*usl3(j)*30

         If(usl3(j).eq.0) CHBIso(j)=0
         If(usl3(j).eq.1) CHBIso(j)=(HSBIO0(j)+HSBIO(j)+HSBIO1(j)+HSBIO2(j)+HSBIO3(j))*30*0.33

         If(usl3(j).eq.2) CHBIso(j)=(HSBIO0(j)+HSBIO(j)+HSBIO1(j)+HSBIO2(j)+HSBIO3(j))*30*0.66

         If(usl3(j).eq.3) CHBIso(j)=(HSBIO0(j)+HSBIO(j)+HSBIO1(j)+HSBIO2(j)+HSBIO3(j))*30*1.00


         If(usl3(j).eq.0) CHHUso(j)=0 

         If(usl3(j).eq.1) CHHUso(j)=(HHUM00(j)+HHUM0(j)+HHUM1(j)+HHUM2(j)+HHUM3(j))*30*0.33

         If(usl3(j).eq.2) CHHUso(j)=(HHUM00(j)+HHUM0(j)+HHUM1(j)+HHUM2(j)+HHUM3(j))*30*0.66

         If(usl3(j).eq.3) CHHUso(j)=(HHUM00(j)+HHUM0(j)+HHUM1(j)+HHUM2(j)+HHUM3(j))*30*1.00



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

         CHFBI0(j)=FDBIO(j)*rmW3(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.66*0.00278))*(1-(0.85/(1+rE(j))))
         if(hgr(j) < -20)CHFBI0(j)=0

         CHFHU0(j)=FDHUM(j)*rmW3(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.02*0.00278))*(1-0.85*(0.85/(1+rE(j))))

         if(hgr(j) < -20)CHFHU0(j)=0

c============================================================
c     RASCHET  RAZLOGJENIJ  FRPM  UDOBRENIY
c============================================================
       	FBIO1(j)=FRPM(j)*(1/(1+ratX(j)))*0.46
         FHUM1(j)=FRPM(j)*(1/(1+ratX(j)))*0.54

         FR1CO(j)=FRPM(j)-(FBIO1(j)+FHUM1(j))

         CHFBI1(j)=FBIO1(j)*rmW3(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.66*0.00278))*(1-(0.85/(1+rE(j))))
         if(hgr(j) < -20)CHFBI1(j)=0

         CHFHU1(j)=FHUM1(j)*rmW3(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.02*0.00278))*(1-0.85*(0.85/(1+rE(j))))

         if(hgr(j) < -20)CHFHU1(j)=0
c============================================================
c     RASCHET  RAZLOGJENIJ  FHUM  UDOBRENIY
c============================================================
	      FBIO3(j)=FHUM(j)*(1/(1+ratX(j)))*0.46
         FHUM3(j)=FHUM(j)*(1/(1+ratX(j)))*0.54

         FR3CO(j)=FHUM(j)-(FBIO3(j)+FHUM3(j))


         CHFBI3(j)=FBIO3(j)*rmW3(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.66*0.00278))*(1-(0.85/(1+rE(j))))
         if(hgr(j) < -20)CHFBI3(j)=0

         CHFHU3(j)=FHUM3(j)*rmW3(j)*(exp(-rmt1(j)*rmpH1(j)*0.6*0.02*0.00278))*(1-0.85*(0.85/(1+rE(j))))

         if(hgr(j) < -20)CHFHU3(j)=0

c==============================================================
c  Summarnoe razlogenie po vsem komponentam udobreniy
c============================================================
         BIOfum(j)=FDBIO(j)+FBIO1(j)+FBIO3(j)
         HUMfum(j)=FDHUM(j)+FHUM1(j)+FHUM3(j)

	      SMCfum(j)=(BIOfum(j)+HUMfum(j))*30

         SMNfum(j)=SMCfum(j)*1000/rnitr(5)

c        if(j.gt.1) SMNfum(j)=SMCfum(j)/CNfum(j)  

         If(usl3(j).eq.0) CO2fum(j)=((FDCO(j)+FR1CO(j)+FR3CO(j))*30)

         If(usl3(j).eq.1) CO2fum(j)=((FDCO(j)+FR1CO(j)+FR3CO(j))*30)*0.5*0.8

         If(usl3(j).eq.2) CO2fum(j)=((FDCO(j)+FR1CO(j)+FR3CO(j))*30)*0.5*0.6

         If(usl3(j).eq.3)  CO2fum(j)=((FDCO(j)+FR1CO(j)+FR3CO(j))*30)*0.5*0.4

	      SMCOfm=SMCOfm+CO2fum(j)
        
c        CHBIfm(j)=CHFBI0(j)*usl3(j)*30+CHFBI1(j)*usl3(j)*30+CHFBI3(j)
c     4 *usl3(j)*30
c        CHHUfm(j)=CHFHU0(j)*usl3(j)*30+CHFHU1(j)*usl3(j)*30+CHFHU3(j)
c     4 *usl3(j)*30


         If(usl3(j).eq.0)CHBIfm(j)=0
 
         If(usl3(j).eq.1) CHBIfm(j)=(CHFBI0(j)+CHFBI1(j)+CHFBI3(j))*30*0.33

         If(usl3(j).eq.2) CHBIfm(j)=(CHFBI0(j)+CHFBI1(j)+CHFBI3(j))*30*0.66

         If(usl3(j).eq.3) CHBIfm(j)=(CHFBI0(j)+CHFBI1(j)+CHFBI3(j))*30*1.00

        
         If(usl3(j).eq.0) CHHUfm(j)=0

         If(usl3(j).eq.1) CHHUfm(j)=(CHFHU0(j)+CHFHU1(j)+CHFHU3(j))*30*0.33

         If(usl3(j).eq.2) CHHUfm(j)=(CHFHU0(j)+CHFHU1(j)+CHFHU3(j))*30*0.66
 
         If(usl3(j).eq.3) CHHUfm(j)=(CHFHU0(j)+CHFHU1(j)+CHFHU3(j))*30*1.00





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
	      PoleC(j)=(BIOrst(j)+HUMrst(j)+BIOsoi(j)+HUMsoi(j)+BIOfum(j)+HUMfum(j))*30
        
ccccc        PolCO2(j)=CO2rst(j)+CO2soi(j)+CO2fum(j)
cccc       PolCO2(j)
         SPolCO=SMCOrs+SMCOso+SMCOfm
c==============================================================
c            GODOVIE  SUMMI
c==============================================================
c     RASCHET ZA GOD RAZLOGJENIJ  otdelnix komponentov RASTITELNIX OSTATKOV
c             i  otnoschenij  C/N
c==============================================================
         SmDBIO=SmDBIO+DBIO(j)*30+PrDBIO(j)*30
         SmBIO1=SmBIO1+BIO1(j)*30+PrBIO1(j)*30
         SmBIO2=SmBIO2+BIO2(j)*30+PrBIO2(j)*30
         SmBIO3=SmBIO3+BIO3(j)*30+PrBIO3(j)*30

         SmDHUM=SmDHum+DHUM(j)*30+PrDHUM(j)*30
         SmHUM1=SmHum1+HUM1(j)*30+PrHUM1(j)*30
         SmHUM2=SmHum2+HUM2(j)*30+PrHUM2(j)*30
         SmHUM3=SmHum3+HUM3(j)*30+PrHUM3(j)*30

         TSMCrs=TSMCrs+SMCrst(j)
         TSMNrs=TSMNrs+SMNrst(j)
ccc&&&&&&         SMNrst(j)=rnitr(3)
         OCNrst=rnitr(3)
c         if(j.gt.1) SMNrst(j)=SMCrst(j)/CNrst(j)
ccccccccccc          if(j.gt.1) OCNrst=SMCrst(j)/CNrst(j)  
ccccccccccccc	  OCNrst=SMCrst(j)/CNrst(j)

ccccccccc         CNrst(j)=TSMCrs/TSMNrs

         SmDCO=SmDCO+(DCO2(j)+PrDCO2(j))*30
         SmR1CO=SmR1CO+(R1CO2(j)+PrR1CO(j))*30
         SmR2CO=SmR2CO+(R2CO2(j)+PrR2CO(j))*30
         SmR3CO=SmR3CO+(R3CO(j)+PrR3CO(j))*30


c	 if(j.gt.1)go to 2001
cccccccccccccc	SMNrst(j)=SMCrst(j)*1000/rnitr(3)*30

	      SMNrst(j)=(SMCrst(j)*1000/rnitr(3))*30


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
 2002    SsDBIO=SsDBIO+SDBIO(j)*30
         SsBIO1=SsBIO1+SBIO1(j)*30
         SsBIO2=SsBIO2+SBIO2(j)*30
         SsBIO3=SsBIO3+SBIO3(j)*30
   
         SsDHUM=SsDHUM+SDHUM(j)*30
         SsHUM1=SsHUM1+SHUM1(j)*30
         SsHUM2=SsHUM2+SHUM2(j)*30
         SsHUM3=SsHUM3+SHUM3(j)*30
   
         TSMCso=TSMCso+SMCsoi(j)
         TSMNso=TSMNso+SMNsoi(j)
cccc       CNsoil(j)=TSMCso/TSMNso
         CNsoil(j)=rNitr(4)
         SsDCO=SsDCO+SDCO(j)*30
         SsR1CO=SsR1CO+sR1CO(j)*30
         SsR2CO=SsR2CO+sR2CO(j)*30
         SsR3CO=SsR3CO+sR3CO(j)*30

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
       
         SFDCO=SFDCO+FDCO(j)*30
         SFR1CO=SFR1CO+FR1CO(j)*30

         SFR3CO=SFR3CO+FR3CO(j)*30

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

         RNupt(j)=0.75*(CUrost(j)/rnitr(3))*1000*rnitr(11)


         VNnitr(j)=(rMNsoi(j)+(SMNrst(j)+SMNsoi(j)+SMNfum(j)-RNupt(j))*0.8)*exp(-0.6*rmt2(j)*rmW2(j)*rmpH2(j))+rnitr(7)*rnitr(8)

         if(Tpoch(j) <= 0) VNnitr(j)=0




c      Primechanie  20% visvobogjdaemogo azota vkljuchaetsj v stabilhuju funkziju organicheskogo veschestva
c           str/258 Gollandzi


c	VNnitr(j)=(rnitr(1)*exp(-0.6*rmt2(j)*rmW2(j)*rmpH2(j))+
c     4 SMNrst(j)+SMNsoi(j)+SMNfum(j)+rnitr(7)*rnitr(8))*30
c        if(Tpoch(j).lt.0.or.Tpoch(j).eq.0) VNnitr(j)=0

c   	VNnitr(j)=NNH4

ccccccccc        VNN2O(j)=rmt2(j)*rmW2(j)*rmpH2(j)*(0.004+0.030*rMNsoi(j))

         If(usl3(j).eq.0) VNN2O(j)=(rmt2(j)*rmW2(j)*rmpH2(j)*(0.004+0.030*VNnitr(j)))*30

        
         If(usl3(j).eq.1) VNN2O(j)=(rmt2(j)*rmW2(j)*rmpH2(j)*(0.004+0.030*VNnitr(j)))*30*0.66
   


         If(usl3(j).eq.2) VNN2O(j)=(rmt2(j)*rmW2(j)*rmpH2(j)*(0.004+0.030*VNnitr(j)))*30*0.33

         If(usl3(j).eq.3) VNN2O(j)=(rmt2(j)*rmW2(j)*rmpH2(j)*(0.004+0.030*VNnitr(j)))*30*0.01

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

         If(usl3(j).eq.0)vNdN2(j)=(FdNO3(j)*FdWFPS(j)*30)/1000

         If(usl3(j).eq.1)vNdN2(j)=((FdNO3(j)*FdWFPS(j)*30)/1000)*0.66

         If(usl3(j).eq.2)vNdN2(j)=((FdNO3(j)*FdWFPS(j)*30)/1000)*0.33

         If(usl3(j).eq.3)vNdN2(j)=((FdNO3(j)*FdWFPS(j)*30)/1000)*0.01


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
c          'inf(7) - nomer pervogo mesjza vegetazii kulturi'

c       CUrost(j)=((2.3026*(2./3)*10.**(2.-(2./3)*
c     6 (j-inf(7)+1)))/(1.+10.**(2.-(2./3)*(j-inf(7)+1)))**2)*SumRst

c       if(j.lt.inf(7))CUrost(j)=0
c       if(j.gt.(inf(7)+inf(5)))CUrost(j)=0

cc================================================================






         OtnUrs(j)=((2.3026*(2./3)*10.**(2.-(2./3)*(j-inf(7)+1)))/(1.+10.**(2.-(2./3)*(j-inf(7)+1)))**2)
         
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
         RNNH4=RNNH4+VNN2O(j)*30
c	BNNH4(j)=NNH4+SMNrst(j)+SMNsoi(j)+SMNfum(j)+rnitr(7)
c     4 *rnitr(8)
c     4   -rnitr(10)*RNupt(j)-VNN2O(j)-vNNO(j)-VNvfum(j)-Vnvfrt(j)




c	NNH4=NNH4+10
c        NNO3=NNO3+VNnitr(j)-VNdeni(j)-rnitr(11)*RNupt(j)-VNdN2(j)-
c     4   VNdN2O(j)-RNinfl(j)
	      RNNO3=RNNO3+(vNdN2(j)*30/1000)
        
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
c        SMCHPL=SMCHPL+rastCH+soilCH+fumCH
         SMCHPL=SMCHPL+CHrst(j)+CHsoil(j)+CHfum(j)    
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
	
c       write(OutputFileUnit, *) 'inf(10) - otnositelniy vinos azota s urogjaem '
c       write(OutputFileUnit, *) 'inf(4) = :1- ozim pscheniza (0.035),2 - jachmen(0.030) '
c       write(OutputFileUnit, *) ' 3 - gorox (0.016)4 - kukuruza na zerno (0.030)
c      5  5 - kukuruza na silos (0.003)'
c       write(OutputFileUnit, *) '6 - podsolnechnic(0.037)  7 - ljuzerna (0.005)
c      5 , 0 -  par'

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

c       write(OutputFileUnit, *) 'inf(10)= 1-tjgjeliy suglinok; 2- sredniy suglinok:
c      3 3 legkiy suglinok; 4- supeschanaj pochva; 5 peschanaj pochva'

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
         rdplCH(j)=SMCHPL  
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

      j1=j-1
        write(OutputFileUnit,121)

          write(OutputFileUnit,9371)
9371  format(4x,76('='))
       write(OutputFileUnit, *) '                       T A B L I Z A  R.1     '
       write(OutputFileUnit, *) '                       T A B L I Z A  R.1     '
       write(OutputFileUnit,970)
         write(OutputFileUnit,9321)
           write(OutputFileUnit,9311)
9321  format(4x,76('='))
 970  format(10x,' RASCHET  DEFIZITA  WLAGI ')
9311  format(4x,76('-'))
        write(OutputFileUnit,143)
 143   format(1x,'i','dek','i','cyt','i',1x,'Tisp  ',4x,'i',
     4 3x,'ratX',2x,'i',2x,'pBIO',1x,'i',3x,'pHUM',4x,'i',
     5 3x,' pCO2',4x,'i',2x,   'rE  i')

       write(OutputFileUnit,120)
c=================
      do 154 j=1,n
      write(OutputFileUnit,151)j,gim(j),Tisp(j),ratX(j),pBIO(j),pHUM(j),
     3 pCO2(j),rE(j)

  154 continue
 151   format(1x,'i',i3,' i',i4,' i',6(f7.3,1x,' i'))
      write(OutputFileUnit,121)
       write(OutputFileUnit, *) '                       T A B L I Z A  R.2'
       write(OutputFileUnit, *) '                       T A B L I Z A  R.2     '     
c====================================================
      write(OutputFileUnit,147)
 147   format(1x,'i','dek','i','cyt','i',1x,'hgr  ',4x,'i',
     4 3x,'Whgr',2x,'i',2x,'rmpH  ',1x,'i',3x,'rmpH1  ',1x,'i',
     5 3x,'rmW   ','i',2x,   'rmW3     i')

      write(OutputFileUnit,120)
c=================
      do 157 j=1,n
      write(OutputFileUnit,155)j,gim(j),hgr(j),Whgr(j),rmpH(j),rmpH1(j),
     3 rmW(j),rmW3(j)

  157 continue
 155   format(1x,'i',i3,'i',i3,'i',6(f7.3,2x,' i'))
      write(OutputFileUnit,121)



c====================================================== 
       write(OutputFileUnit, *) '                       T A B L I Z A  R.2a     '     
c====================================================
      write(OutputFileUnit,447)
 447   format(1x,'i','dek','i','cyt','i',1x,'hgr  ',4x,'i',
     4 3x,'rchW2',2x,'i',2x,'mt1',1x,'i',3x,'mt2   ',1x,'i',
     5 3x,'mW2 1 ','i',2x,   'mpH2     i')

      write(OutputFileUnit,120)
c=================
      do 857 j=1,n
      write(OutputFileUnit,451)j,gim(j),hgr(j),rchW2(j),rmt1(j),rmt2(j),
     3 rmW2(j),rmpH2(j)

  857 continue
 451   format(1x,'i',i3,'i',i3,'i',6(f7.3,2x,' i'))
      write(OutputFileUnit,121)



c====================================================== 




      write(OutputFileUnit,9361)
9361  format(4x,76('='))
      write(OutputFileUnit,120)
       write(OutputFileUnit, *) '                               T A B L I Z A  R.3     '
      write(OutputFileUnit,1250)
      write(OutputFileUnit,4123)
      write(OutputFileUnit,4239)
4102  format(4x,76('='))
1250  format(10x, 'RASCHET  PARAMETROV OSNOVNOGO URAVNENIJ I   ')
4123  format(10x,'  RASTITELNIX OSTATKOV  i DPM i  RPM, t/ga     ')
4239  format(4x,76('='))
      write(OutputFileUnit,120)
      write(OutputFileUnit,5000)
5000   format(1x,'i','dek','i','cyt','i',1x,'raC ',5x,'i',
     4 3x,'rbC',4x,'i',3x,'ratX',3x,'i',3x,'CUrost',3x,'i',6x,'DPM0 i',
     5 2x,'RPM0 i')

      write(OutputFileUnit,120)
      do 45 j=1,n
      write(OutputFileUnit,459)j,gim(j),raC(j),rbC(j),ratX(j),CUrost(j),DPM0(j),
     4 RPM0(j)
459    format(1x,'i',i3,'i',i3,'i',f7.3,3x,'i',5(f7.4,3x,'i'))

 45   continue
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        write(OutputFileUnit,120)
        write(OutputFileUnit,120)
        write(OutputFileUnit, *) '                   T A B L I Z A   R .4  '
        write(OutputFileUnit,120)
        write(OutputFileUnit, *) '              RASCHET  RAZLOGJENIJ   DPM(j)    '
        write(OutputFileUnit, *) '                  RASTITELNIX OSTATKOV, t/ga    '
        write(OutputFileUnit,120)
       write(OutputFileUnit,223)
 223   format(1x,'i','dek','i','cyt','i',3x,'DBIO',11x,'i',
     4 7x,'DHUM',4x,'i',7x,'DCO2',4x,'i',10x,'DPM',2x,'i')
       write(OutputFileUnit,120)
       do 46 j=1,n
       write(OutputFileUnit,224)j,gim(j),DBIO(j),DHUM(j),DCO2(j),DPM(j)
 224  format(1x,'i',i3,'i',i3,'i',2x,f10.9,7x,'i',3(2x,f10.9,2x,'i'))
  46   continue
      write(OutputFileUnit,120)
      write(OutputFileUnit,120)
      write(OutputFileUnit,3361)
3361   format(10x,'                                T A B L I Z A  R.5.')
      write(OutputFileUnit,1142)
       write(OutputFileUnit,120)
1142  format(10x,'             RASCHET  RAZLOGENUJ  RPM(j)')
        write(OutputFileUnit, *) '                RASTITELNIX OSTATKOV, t/ga    '
       write(OutputFileUnit,120)
      write(OutputFileUnit,120)
      write(OutputFileUnit,225)


 225  format(1x,'i','dek','i','cyt','i',3x,'BIO1',11x,'i',
     4 7x,'HUM1',2x,'i',7x,'R1CO2',5x,'i',10x,'RPM',2x,'i')
      write(OutputFileUnit,120)
       do 88 j=1,n
      write(OutputFileUnit,222)j,gim(j),BIO1(j),HUM1(j),R1CO2(j),RPM(j)
 222  format(1x,'i',i3,'i',i3,'i',2x,f10.9,7x,'i',3(2x,f10.9,2x,'i'))
  88  continue
      write(OutputFileUnit,120)
      write(OutputFileUnit,120)
        write(OutputFileUnit, *) '                   T A B L I Z A   R .6  '
       write(OutputFileUnit,120)

        write(OutputFileUnit, *) '            RASCHET  RAZLOGJENIJ   BIO1(j)  '
        write(OutputFileUnit, *) '                RASTITELNIX OSTATKOV, t/ga    '
       write(OutputFileUnit,120)

      write(OutputFileUnit,227)
 227  format(1x,'i','dek','i','cyt','i',1x,'BIO2',9x,'i',3x,'HUM2',7x,
     4'i',3x,'R2CO2',4x,'i',2x,'rdh1+rdh2' )

      write(OutputFileUnit,120)
       do 89 j=1,n
      write(OutputFileUnit,229)j,gim(j),BIO2(j),HUM2(j),R2CO2(j),(rdh1(j)+rdh2(j))
 229  format(1x,'i',i3,'i',i3,'i',4(f10.9,3x,' i'))
  89  continue
      write(OutputFileUnit,120)
c===============================================================
      write(OutputFileUnit,120)
        write(OutputFileUnit, *) '                   T A B L I Z A   R .7  '
       write(OutputFileUnit,120)

        write(OutputFileUnit, *) '            RASCHET  videlenij METANA  '
        write(OutputFileUnit, *) '  IZ   BIO   RASTITELNIX OSTATKOV, t/ga    '
       write(OutputFileUnit,120)

      write(OutputFileUnit,430)
 430  format(1x,'i','dek','i','cyt','i',1x,'CHBIO',9x,'i',3x,'CHBIO1',7x,
     4'i',3x,'CHBIO2',4x,'i',2x,'CHBIO3' )

      write(OutputFileUnit,120)
       do 431 j=1,n
      write(OutputFileUnit,432)j,gim(j),CHBIO(j),CHBIO1(j),CHBIO2(j),CHBIO3(j)
 432  format(1x,'i',i3,'i',i3,'i',4(f10.8,3x,' i'))
 431  continue
      write(OutputFileUnit,120)
c===============================================================

      write(OutputFileUnit,120)

       write(OutputFileUnit,120)
        write(OutputFileUnit, *) '                      T A B L I Z A   R.8       '
       write(OutputFileUnit,120)

        write(OutputFileUnit, *) '       RASCHET  RAZLOGJENIJ   HUM1(j)    '
        write(OutputFileUnit, *) '             RASTITELNIX OSTATKOV, t/ga    '

       write(OutputFileUnit,120)

        write(OutputFileUnit,120)

      write(OutputFileUnit,9324)
      write(OutputFileUnit,9316)
9324  format(4x,76('='))

9316  format(4x,76('-'))
      write(OutputFileUnit,343)
 343   format(1x,'i','dek','i','cyt','i',1x,'BIO3  ',7x,'i',
     4 3x,'HUM3',5x,'i',2x,'R3CO2',7x,'i',3x,'rdh3+rdh4',1x,'i')
       write(OutputFileUnit,120)
          do 174 j=1,n
      write(OutputFileUnit,630)j,gim(j),BIO3(j),HUM3(j),R3CO(j),(rdh3(j)+rdh4(j))

  174 continue
 630   format(1x,'i',i3,'i',i3,'i',4(f10.9,2x,' i'))
      write(OutputFileUnit,120)

c===============================================================
      write(OutputFileUnit,120)
        write(OutputFileUnit, *) '                   T A B L I Z A   R .9  '
       write(OutputFileUnit,120)

        write(OutputFileUnit, *) '            RASCHET  videlenij METANA  '
        write(OutputFileUnit, *) '  IZ   HUM   RASTITELNIX OSTATKOV, t/ga    '
       write(OutputFileUnit,120)

      write(OutputFileUnit,435)
 435  format(1x,'i','dek','i','cyt','i',1x,'CHHUM',9x,'i',3x,'CHHUM1',7x,
     4'i',3x,'CHHUM2',4x,'i',2x,'CHHUM3' )

      write(OutputFileUnit,120)
       do 433 j=1,n
      write(OutputFileUnit,434)j,gim(j),CHHUM(j),CHHUM(j),CHHUM2(j),CHHUM3(j)
 434  format(1x,'i',i3,'i',i3,'i',4(f10.8,3x,' i'))
 433  continue
      write(OutputFileUnit,120)
c===============================================================
c===============================================================
      write(OutputFileUnit,120)
        write(OutputFileUnit, *) '                   T A B L I Z A   R .9a  '
       write(OutputFileUnit,120)

        write(OutputFileUnit, *) '            RASCHET  videlenij CO2  '
        write(OutputFileUnit, *) '  IZ      RASTITELNIX OSTATKOV, t/ga    '
       write(OutputFileUnit,120)

      write(OutputFileUnit,405)
 405  format(1x,'i','dek','i','cyt','i',1x,'DCO2 ',9x,'i',3x,'R1CO2 ',7x,
     4'i',3x,'R2CO2',4x,'i',2x,'R3CO' )

      write(OutputFileUnit,120)
       do 403 j=1,n
      write(OutputFileUnit,404)j,gim(j),DCO2(j),R1CO2(j),R2CO2(j),R3CO(j)
 404  format(1x,'i',i3,'i',i3,'i',4(f10.8,3x,' i'))
 403  continue
      write(OutputFileUnit,120)
c===============================================================


c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        write(OutputFileUnit,120)
        write(OutputFileUnit,120)
        write(OutputFileUnit, *) '                   T A B L I Z A   R .10  '
        write(OutputFileUnit,120)
        write(OutputFileUnit, *) '              RASCHET  RAZLOGJENIJ   PrDPM(j)    '
        write(OutputFileUnit, *) '   RASTITELNIX OSTATKOV  proschlogo goda, t/ga   '
        write(OutputFileUnit,120)
       write(OutputFileUnit,323)
 323   format(1x,'i','dek','i','cyt','i',2x,'PrDBIO',4x,'i',
     4 2x,'PrDHUM',4x,'i',2x,'PrDCO2',4x,'i',2x,'PrDPM',2x,'i')
       write(OutputFileUnit,120)
       do 346 j=1,n
       write(OutputFileUnit,324)j,gim(j),PrDBIO(j),PrDHUM(j),PrDCO2(j),PrDPM(j)
 324  format(1x,'i',i3,'i',i3,'i',2x,f8.7,2x,'i',3(2x,f8.7,2x,'i'))
 346   continue
      write(OutputFileUnit,120)
      write(OutputFileUnit,120)
      write(OutputFileUnit,3366)
3366   format(10x,'                      T A B L I Z A  R.11.')
      write(OutputFileUnit,1182)
       write(OutputFileUnit,120)
1182  format(10x,'             RASCHET  RAZLOGENUJ  PrRPM(j)')
        write(OutputFileUnit, *) '   ASTITELNIX OSTATKOV   proschlogo goda , t/ga  '
       write(OutputFileUnit,120)
      write(OutputFileUnit,120)
      write(OutputFileUnit,325)

 325  format(1x,'i','dek','i','cyt','i',2x,'PrBIO1',4x,'i',
     4 2x,'PrHUM1',4x,'i',2x,'PrR1CO2',4x,'i',2x,'PrRPM',2x,'i')
      write(OutputFileUnit,120)
       do 388 j=1,n
      write(OutputFileUnit,322)j,gim(j),BIO1(j),HUM1(j),R1CO2(j),RPM(j)
 322  format(1x,'i',i3,'i',i3,'i',2x,f8.7,2x,'i',3(2x,f8.7,2x,'i'))
 388  continue
      write(OutputFileUnit,120)
      write(OutputFileUnit,120)
        write(OutputFileUnit, *) '                   T A B L I Z A   R .12  '
       write(OutputFileUnit,120)

        write(OutputFileUnit, *) '            RASCHET  RAZLOGJENIJ   PrBIO1(j)  '
        write(OutputFileUnit, *) '    RASTITELNIX OSTATKOV    proschlogo goda, t/ga  '
       write(OutputFileUnit,120)

      write(OutputFileUnit,327)
 327  format(1x,'i','dek','i','cyt','i',2x,'PrBIO2',2x,'i',2x,'PrHUM2'
     6 ,2x,
     4'i',2x,'PrR2CO2',2x,'i',2x,'Prrdh1+Prrdh2' )

      write(OutputFileUnit,120)
       do 389 j=1,n
      write(OutputFileUnit,329)j,gim(j),PrBIO2(j),PrHUM2(j),PrR2CO(j),
     6 (Prrdh1(j)+Prrdh2(j))
 329  format(1x,'i',i3,'i',i3,'i',4(f8.7,2x,' i'))
 389  continue
      write(OutputFileUnit,120)
      write(OutputFileUnit,120)

       write(OutputFileUnit,120)
        write(OutputFileUnit, *) '                      T A B L I Z A   R.13       '
       write(OutputFileUnit,120)

        write(OutputFileUnit, *) '       RASCHET  RAZLOGJENIJ   PrHUM1(j)    '
        write(OutputFileUnit, *) '     RASTITELNIX OSTATKOV    proschlogo goda, t/ga  '

       write(OutputFileUnit,120)

        write(OutputFileUnit,120)

      write(OutputFileUnit,9114)
      write(OutputFileUnit,9216)
9114  format(4x,76('='))

9216  format(4x,76('-'))
      write(OutputFileUnit,443)
 443   format(1x,'i','dek','i','cyt','i',2x,'PrBIO3  ',2x,'i',
     4 2x,'PrHUM3',2x,'i',2x,'PrR3CO2',2x,'i',2x,'Prrdh3+Prrdh4',1x,'i')
       write(OutputFileUnit,120)
          do 374 j=1,n
      write(OutputFileUnit,333)j,gim(j),PrBIO3(j),PrHUM3(j),PrR3CO(j),
     6 (Prrdh3(j)+Prrdh4(j))
  374 continue
 333   format(1x,'i',i3,'i',i3,'i',4(f8.7,2x,' i'))
      write(OutputFileUnit,120)
c===============================================================
      write(OutputFileUnit,120)
        write(OutputFileUnit, *) '                   T A B L I Z A   R .14  '
       write(OutputFileUnit,120)

        write(OutputFileUnit, *) '            RASCHET  videlenij METANA  '
      write(OutputFileUnit, *) 'IZ  PrBIO  RASTITELNIX OSTATKOV  proschlogo goda, t/ga '
       write(OutputFileUnit,120)

      write(OutputFileUnit,436)
 436  format(1x,'i','dek','i','cyt','i',1x,'PCHBIO',9x,'i',3x,'PCHB1',7x
     4,'i',3x,'PCHB2',4x,'i',2x,'PCHB3' )

      write(OutputFileUnit,120)
       do 437 j=1,n
      write(OutputFileUnit,438)j,gim(j),PCHBIO(j),PCHB1(j),PCHB2(j),PCHB3(j)
 438  format(1x,'i',i3,'i',i3,'i',4(f10.8,3x,' i'))
 437  continue
      write(OutputFileUnit,120)




c===============================================================
c===============================================================
      write(OutputFileUnit,120)
        write(OutputFileUnit, *) '                   T A B L I Z A   R .15  '
       write(OutputFileUnit,120)

        write(OutputFileUnit, *) '            RASCHET  videlenij METANA  '
      write(OutputFileUnit, *) 'IZ  PrHUM  RASTITELNIX OSTATKOV  proschlogo goda, t/ga '
       write(OutputFileUnit,120)

      write(OutputFileUnit,439)
 439  format(1x,'i','dek','i','cyt','i',1x,'PCHHUM',9x,'i',3x,'PCHH1',7x
     4,'i',3x,'PCHH2',4x,'i',2x,'PCHH3' )

      write(OutputFileUnit,120)
       do 440 j=1,n
      write(OutputFileUnit,441)j,gim(j),PCHHUM(j),PCHH1(j),PCHH2(j),PCHH3(j)
 441  format(1x,'i',i3,'i',i3,'i',4(f10.8,3x,' i'))
 440  continue
      write(OutputFileUnit,120)




c===============================================================
c===============================================================
      write(OutputFileUnit,120)
        write(OutputFileUnit, *) '                   T A B L I Z A   R .16  '
       write(OutputFileUnit,120)

        write(OutputFileUnit, *) '  RASCHET  videlenij METANA IZ  RASTITELNIX OSTATKOV '
      write(OutputFileUnit, *) 'IZ  tekuschego i proschlogo goda, t/ga '
       write(OutputFileUnit,120)

      write(OutputFileUnit,442)
 442  format(1x,'i','dek','i','cyt','i',1x,'CHBIrs',9x,'i',3x,'CHHUrs'
     4,7x
     4,'i',3x,'CHrst  ',4x,'i',2x,'SMCHrs' )

      write(OutputFileUnit,120)
       do 445 j=1,n
      write(OutputFileUnit,444)j,gim(j),CHBIrs(j),CHHUrs(j),CHrst(j),rdCHrs(j)
 444  format(1x,'i',i3,'i',i3,'i',4(f10.8,3x,' i'))
 445  continue
      write(OutputFileUnit,120)

c===============================================================



c===================================================
       write(OutputFileUnit,120)
        write(OutputFileUnit, *) '                      T A B L I Z A   R.17       '
       write(OutputFileUnit,120)

        write(OutputFileUnit, *) ' SUMMARNOE   RAZLOGJENIE  PO VSEM KOMPONENTAM'
        write(OutputFileUnit, *) '     RASTITELNIX OSTATKOV, t/ga      '
       write(OutputFileUnit,120)
        write(OutputFileUnit,120)
      write(OutputFileUnit,9224)
      write(OutputFileUnit,9266)
9224  format(4x,76('='))

9266  format(4x,76('-'))
      write(OutputFileUnit,463)
 463   format(1x,'i','dek','i','cyt','i',1x,'BIOrst  ',3x,'i',
     4 3x,'HUMrst',3x,'i',2x,'CO2rst',1x,'i',2x,'CHBIrs',1x,'i',2x,
     4 'CHHUrs',1x,'i',2x,'CHrst',1x,'i')
       write(OutputFileUnit,120)
          do 376 j=1,n
      write(OutputFileUnit,937)j,gim(j),BIOrst(j),HUMrst(j),CO2rst(j),CHBIrs(j),
     4 CHHUrs(j),CHrst(j)
  376 continue
 937   format(1x,'i',i3,'i',i3,'i',6(f8.4,2x,' i'))
      write(OutputFileUnit,120)

c=========================================

c===================================================
       write(OutputFileUnit,120)
        write(OutputFileUnit, *) '                      T A B L I Z A   R.17a       '
       write(OutputFileUnit,120)

        write(OutputFileUnit, *) ' VIBROSI CO2  PROSCHLOGO  GODA'
        write(OutputFileUnit, *) '     RASTITELNIX OSTATKOV, t/ga      '
       write(OutputFileUnit,120)
        write(OutputFileUnit,120)
      write(OutputFileUnit,9274)
      write(OutputFileUnit,9276)
9274  format(4x,76('='))

9276  format(4x,76('-'))
      write(OutputFileUnit,763)
 763   format(1x,'i','dek','i','cyt','i',1x,'prDCO2',3x,'i',
     4 3x,'prR1CO',3x,'i',2x,'prR2CO',1x,'i',2x,'prR3CO',1x,'i')
c,2x,
c     4 'CHHUrs',1x,'i',2x,'CHrst',1x,'i')
       write(OutputFileUnit,120)
          do 776 j=1,n
      write(OutputFileUnit,917)j,gim(j),prDCO2(j),prR1CO(j),prR2CO(j),prR3CO(j)
c     4 CHHUrs(j),CHrst(j)
  776 continue
 917   format(1x,'i',i3,'i',i3,'i',4(f10.8,2x,' i'))
      write(OutputFileUnit,120)

c=========================================




      write(OutputFileUnit,120)
        write(OutputFileUnit, *) '                   T A B L I Z A   R .18  '
       write(OutputFileUnit,120)

        write(OutputFileUnit, *) 'RASCHET  NACHALNIX ZNACHENIY ORGANIKI   '
        write(OutputFileUnit, *) '           POCHVI, t/ga   '
       write(OutputFileUnit,120)


      write(OutputFileUnit,843)
 843   format(1x,'i','dek','i','cyt','i',1x,'SDPM  ',4x,'i',
     4 3x,'SRPM',2x,'i',2x,'SBIO',4x,'i',3x,'SHUM',3x,'i',
     5 3x,' IOM ',2x,'i',2x,   '  SOM  i')
       write(OutputFileUnit,120)
          do 874 j=1,n
      write(OutputFileUnit,851)j,gim(j),SDPM(j),SRPM(j),SBIO(j),SHUM(j),
     3 RIOM(j),inf(8)

  874 continue

 851   format(1x,'i',i3,'i',i3,'i',6(f7.4,2x,' i'))
      write(OutputFileUnit,120)

c============================
      write(OutputFileUnit,120)
        write(OutputFileUnit, *) '                   T A B L I Z A   R .19  '
       write(OutputFileUnit,120)

        write(OutputFileUnit, *) 'RASCHET  ROZLOGJENIJ  ORGANIKI   '
        write(OutputFileUnit, *) '           POCHVI, t/ga   '
       write(OutputFileUnit,120)


      write(OutputFileUnit,831)
 831   format(1x,'i','dek','i','cyt','i',1x,'SDPM0  ',4x,'i',
     4 3x,'SRPM0',2x,'i',2x,'SBIO0',1x,'i',3x,'SHUM0',1x,'i')
       write(OutputFileUnit,120)
          do 674 j=1,n
      write(OutputFileUnit,651)j,gim(j),SDPM0(j),SRPM0(j),SBIO0(j),SHUM0(j)

  674 continue

 651   format(1x,'i',i3,'i',i3,'i',4(f8.6,2x,' i'))
      write(OutputFileUnit,120)
      write(OutputFileUnit,120)

c============================
      write(OutputFileUnit,120)
        write(OutputFileUnit, *) '                   T A B L I Z A   R .20  '
       write(OutputFileUnit,120)

        write(OutputFileUnit, *) '         RASCHET  ROZLOGJENIJ  SDPM0(j)   '
        write(OutputFileUnit, *) '                POCHVI, t/ga   '
       write(OutputFileUnit,120)

      write(OutputFileUnit,43)
  43   format(1x,'i','dek','i','cyt','i',1x,'SDBIO  ',4x,'i',
     4 3x,'SDHUM',2x,'i',2x,'SDCO2',1x,'i')
       write(OutputFileUnit,120)
          do  74 j=1,n
      write(OutputFileUnit,51)j,gim(j),SDBIO(j),SDHUM(j),SDCO(j)

   74 continue
  51   format(1x,'i',i3,'i',i3,'i',3(f10.6,2x,' i'))
      write(OutputFileUnit,120)

c============================
      write(OutputFileUnit,120)
        write(OutputFileUnit, *) '                   T A B L I Z A   R .21 '
       write(OutputFileUnit,120)

        write(OutputFileUnit, *) '        RASCHET  ROZLOGJENIJ  SRPM0(j)   '
        write(OutputFileUnit, *) '                POCHVI, t/ga   '
       write(OutputFileUnit,120)

      write(OutputFileUnit,42)
  42   format(1x,'i','dek','i','cyt','i',1x,'SBIO1  ',4x,'i',
     4 3x,'SHUM1',2x,'i',2x,'SR1CO2',1x,'i')
       write(OutputFileUnit,120)
          do  73 j=1,n
      write(OutputFileUnit,50)j,gim(j),SBIO1(j),SHUM1(j),SR1CO(j)

   73 continue
  50   format(1x,'i',i3,'i',i3,'i',3(f12.10,2x,' i'))
      write(OutputFileUnit,120)

c============================

      write(OutputFileUnit,120)
        write(OutputFileUnit, *) '                   T A B L I Z A   R .22 '
       write(OutputFileUnit,120)

        write(OutputFileUnit, *) '        RASCHET  ROZLOGJENIJ  SBIO0(j)   '
        write(OutputFileUnit, *) '                POCHVI, t/ga   '
       write(OutputFileUnit,120)

      write(OutputFileUnit,41)
  41   format(1x,'i','dek','i','cyt','i',1x,'SBIO2  ',4x,'i',
     4 3x,'SHUM2',2x,'i',2x,'SR2CO2',1x,'i')
       write(OutputFileUnit,120)
          do  72 j=1,n
      write(OutputFileUnit,49)j,gim(j),SBIO2(j),SHUM2(j),SR2CO(j)

   72 continue
  49   format(1x,'i',i3,'i',i3,'i',3(f10.8,2x,' i'))
      write(OutputFileUnit,120)

c============================
      write(OutputFileUnit,120)
        write(OutputFileUnit, *) '                   T A B L I Z A   R .23 '
       write(OutputFileUnit,120)

        write(OutputFileUnit, *) '        RASCHET  ROZLOGJENIJ  SHUM0(j)   '
        write(OutputFileUnit, *) '                POCHVI, t/ga   '
       write(OutputFileUnit,120)

      write(OutputFileUnit,44)
  44   format(1x,'i','dek','i','cyt','i',1x,'SBIO3  ',4x,'i',
     4 3x,'SHUM3',2x,'i',2x,'SR3CO2',1x,'i')
       write(OutputFileUnit,120)
          do  25 j=1,n
      write(OutputFileUnit,418)j,gim(j),SBIO3(j),SHUM3(j),SR3CO(j)

   25 continue
 418   format(1x,'i',i3,'i',i3,'i',3(f10.8,2x,' i'))
      write(OutputFileUnit,120)
c===================================================
c===============================================================
      write(OutputFileUnit,120)
        write(OutputFileUnit, *) '                   T A B L I Z A   R .24  '
       write(OutputFileUnit,120)

        write(OutputFileUnit, *) '            RASCHET  videlenij METANA  '
        write(OutputFileUnit, *) '  IZ   BIO   POCHVI , t/ga    '
       write(OutputFileUnit,120)

      write(OutputFileUnit,450)
 450  format(1x,'i','dek','i','cyt','i',1x,'HSBIO0',9x,'i',3x,'HSBIO',7x,
     4'i',3x,'HSBIO1',4x,'i',2x,'HSBIO2','i',2x,'HSBIO3' )

      write(OutputFileUnit,120)
       do 913 j=1,n
      write(OutputFileUnit,452)j,gim(j),HSBIO0(j),HSBIO(j),HSBIO1(j),HSBIO2(j),
     4 HSBIO3(j)
 452  format(1x,'i',i3,'i',i3,'i',5(f8.7,3x,' i'))
 913  continue
      write(OutputFileUnit,120)
c===============================================================
c===============================================================
      write(OutputFileUnit,120)
        write(OutputFileUnit, *) '                   T A B L I Z A   R .25  '
       write(OutputFileUnit,120)

        write(OutputFileUnit, *) '            RASCHET  videlenij METANA  '
        write(OutputFileUnit, *) '  IZ   HUM   POCHVI , t/ga    '
       write(OutputFileUnit,120)

      write(OutputFileUnit,453)
 453  format(1x,'i','dek','i','cyt','i',1x,'HHUM00',9x,'i',3x,
     4 'HHUM0',7x,
     4'i',3x,'HHUM1',4x,'i',2x,'HHUM2' ,'i',2x,'HHUM3' )

      write(OutputFileUnit,120)
       do 454 j=1,n
      write(OutputFileUnit,455)j,gim(j),HHUM00(j),HHUM0(j),HHUM1(j),HHUM2(j),
     4 HHUM3(j)
 455  format(1x,'i',i3,'i',i3,'i',5(f8.7,3x,' i'))
 454  continue
      write(OutputFileUnit,120)
c===============================================================


       write(OutputFileUnit,120)
        write(OutputFileUnit, *) '                      T A B L I Z A   R.26       '
       write(OutputFileUnit,120)

        write(OutputFileUnit, *) ' SUMMARNOE   RAZLOGJENIE  PO VSEM KOMPONENTAM'
        write(OutputFileUnit, *) '     ORGANIKI  POCHVI, t/ga      '
       write(OutputFileUnit,120)
        write(OutputFileUnit,120)
      write(OutputFileUnit,9204)
      write(OutputFileUnit,9206)
9204  format(4x,76('='))

9206  format(4x,76('-'))
      write(OutputFileUnit,473)
 473   format(1x,'i','dek','i','cyt','i',1x,'BIOsoil  ',7x,'i',
     4 3x,'HUMsoil',5x,'i',2x,'CO2soil',1x,'i')
       write(OutputFileUnit,120)
          do 379 j=1,n
      write(OutputFileUnit,939)j,gim(j),BIOsoi(j),HUMsoi(j),CO2soi(j)
  379 continue
 939   format(1x,'i',i3,'i',i3,'i',3(f10.9,2x,' i'))
      write(OutputFileUnit,120)


c===============================================================
      write(OutputFileUnit,120)
        write(OutputFileUnit, *) '                   T A B L I Z A   R .27  '
       write(OutputFileUnit,120)

        write(OutputFileUnit, *) '            RASCHET  videlenij METANA  '
        write(OutputFileUnit, *) ' SUMMARNO   IZ VSEX BIO i  HUM   POCHVI , t/ga'
        write(OutputFileUnit, *) ' I SUMMARNO   IZ VSEY   POCHVI , t/ga'
       write(OutputFileUnit,120)

      write(OutputFileUnit,456)
 456  format(1x,'i','dek','i','cyt','i',1x,'CHBIso',9x,'i',3x,
     4 'CHHUso',7x,
     4'i',3x,'CHsoil',4x,'i',2x,'SMCHso'  )

      write(OutputFileUnit,120)
       do 457 j=1,n
      write(OutputFileUnit,458)j,gim(j),CHBIso(j),CHHUso(j),CHsoil(j),rdCHso(j)
c     4 HHUM3(j)
 458  format(1x,'i',i3,'i',i3,'i',4(f9.8,3x,' i'))
 457  continue
      write(OutputFileUnit,120)
c===============================================================


c======================
      write(OutputFileUnit,120)
        write(OutputFileUnit, *) '                   T A B L I Z A   R .28 '
       write(OutputFileUnit,120)

        write(OutputFileUnit, *) ' RASCHET  PERVICHNOGO   ROZLOGJENIJ     '
        write(OutputFileUnit, *) '     ORGANICHESKIX  UDOBRENIY, t/ga   '
       write(OutputFileUnit,120)

      write(OutputFileUnit,40)
  40   format(1x,'i','dek','i','cyt','i',1x,'FDPM  ',4x,'i',
     4 3x,'FRPM ',2x,'i',2x,'FHUM  ',1x,'i')
       write(OutputFileUnit,120)
          do  71 j=1,n
      write(OutputFileUnit,48)j,gim(j),FDPM(j),FRPM(j),FHUM(j)

   71 continue
  48   format(1x,'i',i3,'i',i3,'i',3(f10.8,2x,' i'))
      write(OutputFileUnit,120)

c======================
      write(OutputFileUnit,120)
        write(OutputFileUnit, *) '                   T A B L I Z A   R .29 '
       write(OutputFileUnit,120)

        write(OutputFileUnit, *) ' RASCHET     ROZLOGJENIJ       FDPM(j)     '
        write(OutputFileUnit, *) '     ORGANICHESKIX  UDOBRENIY, t/ga   '
       write(OutputFileUnit,120)

      write(OutputFileUnit,420)
 420   format(1x,'i','dek','i','cyt','i',1x,'FDBIO  ',4x,'i',
     4 3x,'FDHUM',2x,'i',2x,'FDCO',1x,'i')
       write(OutputFileUnit,120)
          do  712 j=1,n
      write(OutputFileUnit,482)j,gim(j),FDBIO(j),FDHUM(j),FDCO(j)

 712   continue
 482   format(1x,'i',i3,'i',i3,'i',3(f10.8,2x,' i'))
      write(OutputFileUnit,120)

c======================
      write(OutputFileUnit,120)
        write(OutputFileUnit, *) '                   T A B L I Z A   R .30 '
       write(OutputFileUnit,120)

        write(OutputFileUnit, *) ' RASCHET     ROZLOGJENIJ       FRPM(j)     '
        write(OutputFileUnit, *) '     ORGANICHESKIX  UDOBRENIY, t/ga   '
       write(OutputFileUnit,120)

      write(OutputFileUnit,422)
 422   format(1x,'i','dek','i','cyt','i',1x,'FBIO1  ',4x,'i',
     4 3x,'FHUM1',2x,'i',2x,'FR1CO',1x,'i')
       write(OutputFileUnit,120)
          do  714 j=1,n
      write(OutputFileUnit,484)j,gim(j),FBIO1(j),FHUM1(j),FR1CO(j)

 714   continue
 484   format(1x,'i',i3,'i',i3,'i',3(f10.8,2x,' i'))
      write(OutputFileUnit,120)

c======================
      write(OutputFileUnit,120)
        write(OutputFileUnit, *) '                   T A B L I Z A   R .31 '
       write(OutputFileUnit,120)

        write(OutputFileUnit, *) ' RASCHET     ROZLOGJENIJ       FHUM(j)     '
        write(OutputFileUnit, *) '     ORGANICHESKIX  UDOBRENIY, t/ga   '
       write(OutputFileUnit,120)

      write(OutputFileUnit,428)
 428   format(1x,'i','dek','i','cyt','i',1x,'FBIO3  ',4x,'i',
     4 3x,'FHUM3',2x,'i',2x,'FR3CO',1x,'i')
       write(OutputFileUnit,120)
          do  718 j=1,n
      write(OutputFileUnit,488)j,gim(j),FBIO3(j),FHUM3(j),FR3CO(j)

 718   continue
 488   format(1x,'i',i3,'i',i3,'i',3(f10.8,2x,' i'))
      write(OutputFileUnit,120)
c===================================================
c===============================================================
      write(OutputFileUnit,120)
        write(OutputFileUnit, *) '                   T A B L I Z A   R .32  '
       write(OutputFileUnit,120)

        write(OutputFileUnit, *) '            RASCHET  videlenij METANA  '
        write(OutputFileUnit, *) '  IZ   BIO I  HUM   UDOBRENIY , t/ga    '
       write(OutputFileUnit,120)

      write(OutputFileUnit,460)
 460  format(1x,'i','dek','i','cyt','i',1x,'CHFBI0',9x,'i',3x,
     4 'CHFBI1',7x,
     4'i',3x,'CHFBI3',4x,'i',2x,'CHFHU0','i',2x,'CHFHU1',
     4'i',2x,'CHFHU3' )

      write(OutputFileUnit,120)
       do 461 j=1,n
      write(OutputFileUnit,462)j,gim(j),CHFBI0(j),CHFBI1(j),CHFBI3(j),CHFHU0(j),
     4 CHFHU1(j),CHFHU3(j)
 462  format(1x,'i',i3,'i',i3,'i',6(f8.7,3x,' i'))
 461  continue
      write(OutputFileUnit,120)
c===============================================================
c===============================================================

       write(OutputFileUnit,120)
        write(OutputFileUnit, *) '                      T A B L I Z A   R.33       '
       write(OutputFileUnit,120)

        write(OutputFileUnit, *) ' SUMMARNOE   RAZLOGJENIE  PO VSEM KOMPONENTAM'
        write(OutputFileUnit, *) '     ORGANICHESKIX  UDOBRENIY, t/ga      '
       write(OutputFileUnit,120)
        write(OutputFileUnit,120)
      write(OutputFileUnit,9704)
      write(OutputFileUnit,9706)
9704  format(4x,76('='))

9706  format(4x,76('-'))
      write(OutputFileUnit,773)
 773   format(1x,'i','dek','i','cyt','i',1x,'BIOfum  ',7x,'i',
     4 3x,'HUMfum',5x,'i',2x,'CO2fum',1x,'i')
       write(OutputFileUnit,120)
          do 779 j=1,n
      write(OutputFileUnit,839)j,gim(j),BIOfum(j),HUMfum(j),CO2fum(j)
  779 continue
 839   format(1x,'i',i3,'i',i3,'i',3(f10.9,2x,' i'))
      write(OutputFileUnit,120)
c===============================================================
      write(OutputFileUnit,120)
        write(OutputFileUnit, *) '                   T A B L I Z A   R .34  '
       write(OutputFileUnit,120)

        write(OutputFileUnit, *) '            RASCHET  videlenij METANA  '
        write(OutputFileUnit, *) '  SUMMARNOGO  IZ    UDOBRENIY , t/ga    '
       write(OutputFileUnit,120)

      write(OutputFileUnit,288)
 288  format(1x,'i','dek','i','cyt','i',1x,'CHBIOfm',9x,'i',3x,
     4 'CHHUMfm',7x,
     4'i',3x,'CHfum',4x,'i',2x,'SMCHfum')

      write(OutputFileUnit,120)
       do 465 j=1,n
      write(OutputFileUnit,464)j,gim(j),CHBIfm(j),CHHUfm(j),CHfum(j),rdCHfm(j)
c     4 CHFHU1(j),CHFHU3(j)
 464  format(1x,'i',i3,'i',i3,'i',4(f9.8,3x,' i'))
 465  continue
      write(OutputFileUnit,120)
c===============================================================
c===============================================================
      write(OutputFileUnit,120)
        write(OutputFileUnit, *) '                   T A B L I Z A   R .35  '
       write(OutputFileUnit,120)

        write(OutputFileUnit, *) ' RASCHET  KOEFFIZIENTOV URAVNENIY  '
        write(OutputFileUnit, *) '  RASCHETA NITRIFIKAZII  I DENITRIFIKAZII '
       write(OutputFileUnit,120)

      write(OutputFileUnit,470)
 470  format(1x,'i','dek','i','cyt','i',4x,'denNO3',3x,'i',3x,
     4 'denW',3x,'i',3x,'denCO2',4x,'i',2x,'denpW',4x,'i',2x,
     4 'dnpNO3')
ccc     4'i',2x,'CHFHU3' )

      write(OutputFileUnit,120)
       do 471 j=1,n
      write(OutputFileUnit,472)j,gim(j),denNO3(j),denW(j),denCO2(j),denpW(j),
     4 dnpNO3(j)
 472  format(1x,'i',i3,'i',i3,'i',5(f8.6,3x,' i'))
 471  continue
      write(OutputFileUnit,120)
c===============================================================

c===============================================================
      write(OutputFileUnit,120)
        write(OutputFileUnit, *) '                   T A B L I Z A   R .36  '
       write(OutputFileUnit,120)

        write(OutputFileUnit, *) ' RASCHET  SKOROSII OBRAZOVANIJ UGLERODA  '
cccccccccccccccc        write(OutputFileUnit, *) '  RASCHETA NITRIFIKAZII  I DENITRIFIKAZII '
       write(OutputFileUnit,120)

      write(OutputFileUnit,412)
 412  format(1x,'i','dek','i','cyt','i',1x,'SMCrst',1x,'i',3x,
     4 'SdN2O',7x,'i',3x,'SnN2O',4x,'i',2x,'denpW','i',2x,
     4 'dnpNO3')
ccc     4'i',2x,'CHFHU3' )

      write(OutputFileUnit,120)
       do 413 j=1,n
      write(OutputFileUnit,414)j,gim(j),SMCrst(j),gn2(j),gn1(j),denpW(j),
     4 dnpNO3(j)
 414  format(1x,'i',i3,'i',i3,'i',5(f8.6,3x,' i'))
 413  continue
      write(OutputFileUnit,120)
c===============================================================

c===============================================================
      write(OutputFileUnit,120)
        write(OutputFileUnit, *) '                   T A B L I Z A   R .37  '
       write(OutputFileUnit,120)

        write(OutputFileUnit, *) '     RASCHET    '
       write(OutputFileUnit,120)

      write(OutputFileUnit,133)
 133  format(1x,'i','dek','i','cyt','i',1x,'rNmicr',9x,'i',3x,
     4 'FdNO3',7x,'i',3x,'FdWFPS',4x,'i',2x,'rchW2')
      write(OutputFileUnit,120)
       do 112 j=1,n
      write(OutputFileUnit,113)j,gim(j),rNmicr(j),FdNO3(j),FdWFPS(j),rchW2(j)
ccccccccccc      write(OutputFileUnit,113)j,gim(j),CNrst(j),CNsoil(j),CNfum(j),CNfum(j)
 113  format(1x,'i',i3,'i',i3,'i',4(f8.3,3x,' i'))
 112  continue
      write(OutputFileUnit,120)
c===============================================================

c===============================================================
      write(OutputFileUnit,120)
        write(OutputFileUnit, *) '                   T A B L I Z A   R .38  '
       write(OutputFileUnit,120)

        write(OutputFileUnit, *) '  RASCHET  SKOROSTI MINERALIZAZII - OBRAZOVANIJ  '
        write(OutputFileUnit, *) '        AMMONIJ                 '
       write(OutputFileUnit,120)

      write(OutputFileUnit,476)
 476  format(1x,'i','dek','i','cyt','i',1x,'SMNrst',9x,'i',3x,
     4 'SMNsoi',7x,'i',3x,'SMNfum',4x,'i',2x,'SMNsum')
c,'i',2x,
c     4 'dnpNO3')
ccc     4'i',2x,'CHFHU3' )

      write(OutputFileUnit,120)
       do 477 j=1,n
      write(OutputFileUnit,478)j,gim(j),SMNrst(j),SMNsoi(j),SMNfum(j),SMNfum(j)
c     4 dnpNO3(j)
 478  format(1x,'i',i3,'i',i3,'i',4(f10.5,3x,' i'))
 477  continue
      write(OutputFileUnit,120)
c===============================================================
c===============================================================
      write(OutputFileUnit,120)
        write(OutputFileUnit, *) '                   T A B L I Z A   R .39  '
       write(OutputFileUnit,120)

        write(OutputFileUnit, *) ' RASCHET  NITRIFIKAZII  I EMISSII AZOTA V PROZESSE'
        write(OutputFileUnit, *) '    NITRIFIKAZII - VNnitr(j),VNN2O(j),VNNO(j)'   
             
       write(OutputFileUnit,120)
      write(OutputFileUnit,976)
 976  format(1x,'i','dek','i','cyt','i',1x,'VNnitr',8x,'i',3x,
     4 'VNN2O',7x,'i',3x,'VNNO')
c,4x,'i',2x,'VNdeni','i',3x,'VNdN2',
c     4 4x,'i',2x,'VNdN2O')
      write(OutputFileUnit,120)
       do 377 j=1,n
      write(OutputFileUnit,978)j,gim(j),VNnitr(j),VNN2O(j),VNNO(j)
c,VNdeni(j),
c     4 VNdN2(j),VNdN2O(j)
 978  format(1x,'i',i3,'i',i3,'i',3(f10.6,3x,' i'))
 377  continue
      write(OutputFileUnit,120)
c===============================================================
c===============================================================
      write(OutputFileUnit,120)
        write(OutputFileUnit, *) '                   T A B L I Z A   R .40  '
       write(OutputFileUnit,120)           
        write(OutputFileUnit, *) 'RASCHET  DENITRIFIKAZII  I EMISSII AZOTA V PROZESSE'
        write(OutputFileUnit, *) '    DENITRIFIKAZII - VNdeni(j),VNdN2(j),VNdN2O(j) '
       write(OutputFileUnit,120)
      write(OutputFileUnit,961)
 961  format(1x,'i','dek','i','cyt','i',1x,'VNdeni',8x,'i',3x,'VNdN2+
     4N2O',
     4 7x,'i',3x,'VNdN2O')
      write(OutputFileUnit,120)
       do 907 j=1,n
      write(OutputFileUnit,908)j,gim(j),VNdeni(j),VNdN2(j),VNdN2O(j)
 908  format(1x,'i',i3,'i',i3,'i',3(f10.6,3x,' i'))
 907  continue
      write(OutputFileUnit,120)
c===============================================================
c===============================================================
      write(OutputFileUnit,120)
        write(OutputFileUnit, *) '                   T A B L I Z A   R .41  '
       write(OutputFileUnit,120)           
        write(OutputFileUnit, *) 'RASCHET  POGLOSHENIJ AZOTA RASTENIJMI,'
        write(OutputFileUnit, *) 'INFILTRAZII I VIVETRIVANIJ AZOTA '
        write(OutputFileUnit, *) 'RNupt(j),RNinfl(j),Unvfum(j),Unvfrt(j) '
       write(OutputFileUnit,120)
      write(OutputFileUnit,933)
 933  format(1x,'i','dek','i','cyt','i',1x,'RNupt ',8x,'i',3x,'RNinfl',
     4 7x,'i',3x,'Vnvfum',7x,'i',3x,'Vnvfrt')
      write(OutputFileUnit,120)
       do 977 j=1,n
      write(OutputFileUnit,111)j,gim(j),RNupt(j),RNinfl(j),Vnvfum(j),Vnvfrt(j)
 111  format(1x,'i',i3,'i',i3,'i',4(f10.6,3x,' i'))
 977  continue
      write(OutputFileUnit,120)
c===============================================================


c===============================================================
      write(OutputFileUnit,120)
        write(OutputFileUnit, *) '                   T A B L I Z A   R .42  '
       write(OutputFileUnit,120)

        write(OutputFileUnit, *) ' VIBROSI N2O pri nitrifikazii SumnitrN2O '      
        write(OutputFileUnit, *) ' VIBROSI N2O pri denitrifikazii SumdenN2O'
        write(OutputFileUnit, *) ' (kg N / ga za god) '
       write(OutputFileUnit,120)
      write(OutputFileUnit,161)
 161  format(1x,'i','dek','i','cyt','i',1x,'SumnitrN2O',11x,'i',3x,
     4 'sumdenN2o',
     4 8x,'i',4x,'rMCsoi')
      write(OutputFileUnit,120)
       do 107 j=1,n
      write(OutputFileUnit,108)j,gim(j),VNN2O(j),VNdN2(j),rMCsoi(j)
 108  format(1x,'i',i3,'i',i3,'i',3(f12.6,3x,' i'))
 107  continue
      write(OutputFileUnit,120)
c===============================================================
c===============================================================
      write(OutputFileUnit,120)
        write(OutputFileUnit, *) '                   T A B L I Z A   R .42a  '
       write(OutputFileUnit,120)           
        write(OutputFileUnit, *) 'RASTENIJMI,'
        write(OutputFileUnit, *) 'A '
ccccccccc        write(OutputFileUnit, *) 'SMCOrs,RNinfl(j),Unvfum(j),Unvfrt(j) '
       write(OutputFileUnit,120)
      write(OutputFileUnit,633)
 633  format(1x,'i','dek','i','cyt','i',1x,'SMCOrs ',8x,'i',3x,'SMCOso',
     4 7x,'i',3x,'SMCOfm',7x,'i',3x,'SPolCO')
      write(OutputFileUnit,120)
       do 919 j=1,n
      write(OutputFileUnit,818)j,gim(j),gn4(j),gn5(j),gn6(j),gn7(j)
 818  format(1x,'i',i3,'i',i3,'i',4(f10.6,3x,' i'))
 919  continue
      write(OutputFileUnit,120)
c===============================================================

c===============================================================
      write(OutputFileUnit,120)
        write(OutputFileUnit, *) '                   T A B L I Z A   R .42ab  '
       write(OutputFileUnit,120)           
        write(OutputFileUnit, *) 'RASTENIJMI,'
        write(OutputFileUnit, *) 'A '
       write(OutputFileUnit,120)
      write(OutputFileUnit,639)
 639  format(1x,'i','dek','i','cyt','i',1x,'CUrost ',8x,'i',3x,'RNupt',
     4 7x,'i',3x,'SMCOfm',7x,'i',3x,'SPolCO')
      write(OutputFileUnit,120)
       do 999 j=1,n
      write(OutputFileUnit,888)j,gim(j),CUrost(j),RNupt(j),gn6(j),gn7(j)
 888  format(1x,'i',i3,'i',i3,'i',4(f10.6,3x,' i'))
 999  continue
      write(OutputFileUnit,120)
c===============================================================



c===========================
      write(OutputFileUnit,120)
        write(OutputFileUnit, *) '                   T A B L I Z A   R .42a '
       write(OutputFileUnit,120)

        write(OutputFileUnit, *) ' VIBROSI CO2 '      
        write(OutputFileUnit, *) ' VIBROSI CO2'
c        write(OutputFileUnit, *) ' NNH4,NNO3,TMNN20 '
       write(OutputFileUnit,120)
      write(OutputFileUnit,363)
 363  format(1x,'i','dek','i','cyt','i',1x,' CO2',3x,'i',3x,
     4 'Tpoch',
     4 8x,'i',4x,'rMCsoi')
      write(OutputFileUnit,120)
       do 137 j=1,n
      write(OutputFileUnit,838)j,gim(j),rdg12(j),Tpoch(j),rMCsoi(j)
 838  format(1x,'i',i3,'i',i3,'i',3(f12.6,3x,' i'))
 137  continue
      write(OutputFileUnit,120)
c===============================================================


c===================================================
       write(OutputFileUnit,120)
        write(OutputFileUnit, *) '                      T A B L I Z A   R.43       '
       write(OutputFileUnit,120)
c============================================================
c  Nnakoplenie ugleroda na pole i vibrosi CO2: PolcC(j) i  PolCO2(j)

        write(OutputFileUnit, *) ' SUMMARNOE   NAKOLENIE  UGLERODA NA POLE(PoleC(j)'
        write(OutputFileUnit, *) 'I  VIBROSI  CO2 (PolCO2(j), t/ga      '
c       write(OutputFileUnit,120)
c        write(OutputFileUnit,120)
c      write(OutputFileUnit,9304)
      write(OutputFileUnit,9306)
c9304  format(4x,76('='))

9306  format(4x,76('-'))
      write(OutputFileUnit,783)
 783   format(1x,'i','dek','i','cyt','i',1x,'PoleC  ',7x,'i',
     4 3x,'PoleCO2',5x,'i')
       write(OutputFileUnit,120)
          do 789 j=1,n
      write(OutputFileUnit,889)j,gim(j),PoleC(j),gn7(j)
  789 continue
 889   format(1x,'i',i3,'i',i3,'i',2(f10.9,2x,' i'))
      write(OutputFileUnit,120)


c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



      write(OutputFileUnit,120)
       write(OutputFileUnit, *) 'RASCHET ZA GOD RAZLOGJENIJ RASTITELNIX OSTATKOV, t/ga;'
      write(OutputFileUnit,120)
      write(OutputFileUnit,848)rd1(n),rd2(n),rd3(n),rd4(n),rd5(n),rd6(n),rd7(n),
     4 rd8(n),rd9(n),rd10(n),rd11(n),rd12(n)

 848   format(1x,'SmDBIO=',f7.3,1x,'SmBIO1=',f7.3,1x,'SmBIO2=',f7.3,1x,
     4 'SmBIO3=',f7.3,1x,'SmDHUM=',f7.3,1x,'SMHUM1=',f7.3,1x,
     4 'SMHUM2=',f7.3,1x,'SMHUM3=',f7.3,1x,'SmDCO=',f7.3,1x,'SMR1CO='
     4 ,f7.3,5x,'SMR2CO=',f7.3,1x,'SMR3CO=',f7.3)
      write(OutputFileUnit,120)
       write(OutputFileUnit, *) 'RASCHET ZA GOD RAZLOGJENIJ ORGANIKI POCHVI, t/ga ;'
      write(OutputFileUnit,120)
      write(OutputFileUnit,878)rds1(n),rds2(n),rds3(n),rds4(n),rds5(n),rds6(n),
     4 rds7(n),rds8(n),rds9(n),rds10(n),rds11(n),rds12(n)
c 827 4 format(1x,'SmDBIO=',f10.5)
878   format(1x,'SsDBIO=',f7.5,1x,'SsBIO1=',f7.5,1x,'SsBIO2=',f7.5,1x,
     4 'SsBIO3=',f7.5,1x,'SsDHUM=',f7.5,1x,'SsHUM1=',f7.5,1x,
     4 'SsHUM2=',f7.5,1x,'SsHUM3=',f7.5,1x,'SsDCO=',f7.5,1x,'SsR1CO='
     4 ,f7.5,5x,
     4 'SsR2CO=',f7.5,1x,'SsR3CO=',f7.5)
      write(OutputFileUnit,120)
      write(OutputFileUnit,120)
       write(OutputFileUnit, *) 'RASCHET ZA GOD RAZLOGJENIJ ORGANIKI UDOBRENIY, t/ga;'
      write(OutputFileUnit,120)
      write(OutputFileUnit,879)rdf1(n),rdf2(n),rdf3(n),rdf4(n),rdf5(n),rdf6(n),
     4 rdf7(n),rdf8(n),rdf9(n)
c 827 4 format(1x,'SmDBIO=',f10.5)
 879   format(1x,'SFDBIO=',f7.5,1x,'SFBIO1=',f7.5,1x,
     4 'SFBIO3=',f7.5,1x,'SFDHUM=',f7.5,1x,'SFHUM1=',f7.5,1x,
     4 'SFHUM3=',f7.5,1x,'SFDCO=',f7.5,1x,'SFR1CO='
     4 ,f7.5,5x,'SFR3CO=',f7.5)
      write(OutputFileUnit,120)

      write(OutputFileUnit,120)
      write(OutputFileUnit,120)
       write(OutputFileUnit, *) 'RASCHET ZA GOD RAZLOGJENIJ RASTITELNIX OSTATKOV,;'
       write(OutputFileUnit, *) '   VSEX KOMPONENTOV, t/ga ;'
      write(OutputFileUnit,120)
      write(OutputFileUnit,900)rdg1(n),rdg2(n),rdg3(n)

 900   format(1x,'RastBIO =',f7.6,1x,'RastHUM =',f7.6,1x,
     4 'RastCO2 =',f10.8,1x)
      write(OutputFileUnit,120)
      write(OutputFileUnit,120)


      write(OutputFileUnit,120)
      write(OutputFileUnit,120)
       write(OutputFileUnit, *) 'RASCHET ZA GOD NERAZLOGIVSHIXSJ RASTIT OSTATKOV, t/ga;'
       write(OutputFileUnit, *) '  ;'
      write(OutputFileUnit,120)
      write(OutputFileUnit,876)rmgw4(n),drww1(j)

 876   format(1x,'SumDPM =',f12.10,1x,'prSMCO =',f12.10,1x)
      write(OutputFileUnit,120)
      write(OutputFileUnit,120)


       write(OutputFileUnit, *) 'RASCHET ZA GOD RAZLOGJENIJ ORGANIKI POCHVI,;'
       write(OutputFileUnit, *) '   VSEX KOMPONENTOV, t/ga ;'
      write(OutputFileUnit,120)
      write(OutputFileUnit,901)rdg4(n),rdg5(n),rdg6(n)

 901   format(1x,'SoilBIO =',f7.5,1x,'SoilHUM =',f7.5,1x,
     4 'SoilCO2 =',f10.8)
      write(OutputFileUnit,120)
      write(OutputFileUnit,120)
       write(OutputFileUnit, *) 'RASCHET ZA GOD RAZLOGJENIJ ORGANIKI UDOBRENIY,;'
       write(OutputFileUnit, *) '   VSEX KOMPONENTOV, t/ga ;'
      write(OutputFileUnit,120)
      write(OutputFileUnit,922)rdg7(n),rdg8(n),rdg9(n)


 922   format(1x,'FumBIO =',f7.5,1x,'FumHUM =',f7.5,1x,
     4 'FUMCO2 =',f10.8)
      write(OutputFileUnit,120)
      write(OutputFileUnit,120)
       write(OutputFileUnit, *) 'RASCHET ZA GOD RAZLOGJENIJ Rastitenix ostatkov,;'
       write(OutputFileUnit, *) 'organiki pochvi i organicheskogo veschestva ;'
       write(OutputFileUnit, *) 'organicheskix udobreniy na pole proekta, t/ga ;'
      write(OutputFileUnit,120)
c      write(OutputFileUnit,922)rdg10(n),rdg11(n),rdg12(n)
      write(OutputFileUnit,902)rdg10(n),rdg11(n),rmg1(n)


 902   format(1x,'SumBIO pole =',f7.5,1x,'SumHUM pole =',f7.5,1x,
     4 'SSSHum =',f7.5)
      write(OutputFileUnit,120)

      write(OutputFileUnit,120)
       write(OutputFileUnit, *) 'RASCHET ZA GOD VIDELENIJ METANA IZ Rastitenix 
     4 ostatkov,;'
       write(OutputFileUnit, *) 'organiki pochvi i organicheskogo veschestva ;'
       write(OutputFileUnit, *) 'organicheskix udobreniy na pole proekta, t/ga ;'
      write(OutputFileUnit,120)
      write(OutputFileUnit,932)rdrsCH(n),rdsoCH(n),rdfmCH(n)


 932   format(1x,'rastCH pole =',f7.5,1x,'soilCH pole =',f7.5,1x,
     4 'fumCH pole =',f7.5)
      write(OutputFileUnit,120)


c      write(OutputFileUnit,120)
      write(OutputFileUnit,120)
       write(OutputFileUnit, *) 'VIDILENIE METANA iz  POLE  PROEKTA za god;'
       write(OutputFileUnit,837)rdplCH(n)*1000
 837  format(1x,'VIDILENIE  METANA  , kg CH4/ ga =',f10.5)
      write(OutputFileUnit,120)

c      write(OutputFileUnit,120)
      write(OutputFileUnit,120)
       write(OutputFileUnit, *) 'VIDILENIE CO2 iz  POLE  PROEKTA za god;'
       write(OutputFileUnit,828)gn7(n)
 828  format(1x,'VIDILENIE CO2  , t C /ga =',f10.5)
      write(OutputFileUnit,120)
c      write(OutputFileUnit,120)

c      write(OutputFileUnit,120)
      write(OutputFileUnit,120)
       write(OutputFileUnit, *) 'VIDILENIE DIOKSIDA UGLERODA - CO2 iz  POLE 
     4  PROEKTA za god;'
       write(OutputFileUnit,228)(gn7(n)*3.67)
 228  format(1x,'VIDILENIE DIOKSIDA UGLERODA - CO2  , t/ga =',f10.5)
      write(OutputFileUnit,120)
c      write(OutputFileUnit,120)

c      write(OutputFileUnit,120)
      write(OutputFileUnit,120)
       write(OutputFileUnit, *) 'VIDILENIE N2O iz  POLE  PROEKTA za god;'
       write(OutputFileUnit,238)gn3(n)
 238  format(1x,'VIDILENIE N2O  , kg N /ga =',f10.5)
      write(OutputFileUnit,120)
c      write(OutputFileUnit,120)



       write(OutputFileUnit, *) 'PRIBAVKA UGLERODA za schet Razlogenij za god;'
       write(OutputFileUnit,841)(rdg10(n)+rdg11(n)+rmg1(n))

 841  format(1x,'PRIBAVKA UGLEROD za schet Razlogenij, t/ga =',f10.5)
      write(OutputFileUnit,120)

       write(OutputFileUnit, *) 'VINOS  UGLEROGA S  MASSOY  UROGJAJ za god;'
       write(OutputFileUnit,845)rmgw1(n)
 845  format(1x,' VINOS  UGLEROGA S  MASSOY  UROGJAJ , t/ga =',f10.5)
      write(OutputFileUnit,120)

       write(OutputFileUnit, *) 'SUMMARNOE PRIRASCHENIE  UGLEROGA za god;'
       write(OutputFileUnit, *) ' s uchetom vibrosov CO2 pochvi i vinosa massoy urogaj
     4  za god;'
       write(OutputFileUnit,846)(rdg10(n)+rdg11(n)+rmg1(n)-rdg6(n)-rmgw1(n))

 846  format(1x,'SUMMARNOE PRIRASCHENIE UGLEROGA na pole,t/ga =',f10.5)
      write(OutputFileUnit,120)

       write(OutputFileUnit, *) 'BALANS  UGLERODA NA POLE  PROEKTA;'
       write(OutputFileUnit,827)rdg13(n)
 827  format(1x,'BALANS UGLERODA , t/ga =',f10.5)
      write(OutputFileUnit,120)
      write(OutputFileUnit,120)

c      zapis  v maliy fail
c      malo
      write(7,120)
      write(7,9117)
 9117 format(10x,'RASCHET ZA GOD RAZLOGJENIJ RASTITELNIX OSTATKOV,;')
      write(7,9118)
 9118 format(10x,'   VSEX KOMPONENTOV, t/ga ;')
      write(7,120)
      write(7,900)rdg1(n),rdg2(n),rdg3(n)
c      malo
c      malo
      write(7,120)
      write(7,9119)
 9119 format(10x,'RASCHET ZA GOD NERAZLOGIVSHIXS RASTIT OSTATKOV,t/ga;')
      write(7,120)
      write(7,876)rmgw4(n)
c      malo
c      malo
      write(7,120)
      write(7,9120)
 9120 format(10x,'RASCHET ZA GOD RAZLOGJENIJ ORGANIKI POCHVI,;')
      write(7,9121)
 9121 format(10x,'   VSEX KOMPONENTOV, t/ga ;')
      write(7,120)
      write(7,901)rdg4(n),rdg5(n),rdg6(n)
      write(7,120)
c      malo
c      malo
      write(7,9122)
 9122 format(10x,'RASCHET ZA GOD RAZLOGJENIJ ORGANIKI UDOBRENIY,;')
      write(7,9123)
 9123 format(10x,'   VSEX KOMPONENTOV, t/ga ;')
      write(7,120)
      write(7,922)rdg7(n),rdg8(n),rdg9(n)
      write(7,120)
c      malo
c      malo
      write(7,9124)
 9124 format(10x,'RASCHET ZA GOD RAZLOGJENIJ Rastitenix ostatkov,;')
      write(7,9125)
 9125 format(10x,'organiki pochvi i organicheskogo veschestva ;')
      write(7,9126)
 9126 format(10x,'organicheskix udobreniy na pole proekta, t/ga ;')
      write(7,120)
      write(7,902)rdg10(n),rdg11(n),rmg1(n)
      write(7,120)
c      malo
c      malo
      write(7,9127)
 9127 format(10x,'VIDILENIE CO2 iz  POLE  PROEKTA za god, t/ga;')
       write(7,828)rdg12(n)
      write(7,120)
c      malo
c      malo
      write(7,9128)
 9128 format(10x,'PRIBAVKA UGLERODA za schet Razlogenij za god, t/ga;')
       write(7,841)(rdg10(n)+rdg11(n)+rmg1(n))
      write(7,120)
c      malo
c      malo
      write(7,9129)
 9129 format(10x,'VINOS  UGLEROGA S  MASSOY  UROGJAJ za god, t/ga;')
       write(7,845)rmgw1(n)
      write(7,120)
c      malo
c      malo
      write(7,9130)
 9130 format(10x,'SUMMARNOE PRIRASCHENIE  UGLEROGA za god, t/ga;')
cccccccccc       write(7,846)(rdg10(n)+rdg11(n)+rmg1(n)-rdg6(n)-rmgw1(n))
       write(7,846)(rdg10(n)+rdg11(n)+rmg1(n)-gn7(n)-rmgw1(n))

      write(7,120)
c      malo
c      malo
      write(7,9131)
 9131 format(10x,'BALANS  UGLERODA NA POLE  PROEKTA, t/ga;')
       write(7,827)rdg13(n)
      write(7,120)
c      malo

c      zapis  v ochen maliy fail
c      ochen malo
      write(8,9132)
 9132 format(10x,'SUMMARNOE PRIRASCHENIE  UGLEROGA za god, t/ga;')
       write(8,846)(rdg10(n)+rdg11(n)+rmg1(n)-rdg6(n)-rmgw1(n))
      write(8,120)
c      ochen malo
c      ochen malo
      write(8,9133)
 9133 format(10x,'BALANS  UGLERODA NA POLE  PROEKTA, t/ga;')
       write(8,827)rdg13(n)
      write(8,120)


c      write(8,9373)
c 9373 format(10x,'BALANS  UGLERODA NA POLE  PROEKTA, t/ga;')
c       write(8,827)SMNrst(1)
c      write(8,120)

c      ochen malo


c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      close (unit=OutputFileUnit)
      close (unit=7)
      close (unit=8)
      
      end

