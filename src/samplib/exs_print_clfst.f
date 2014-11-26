C
C
C
C
C
C+
       SUBROUTINE EXS_PRINT_CLFST (OPTION, STATUS)
C      -------------------------------------------
C
C  Prints observing parameters for CLFST sample files.
C
C  The STATUS value should be zero on entry.
C
*-
       CHARACTER  OPTION*(*)
       INTEGER    STATUS
C
       CHARACTER  STRING*60
       CHARACTER  CHRA*16, CHDEC*16, CDATE*11, LTIME*3
       INTEGER    IDATE(3), IPRINT, ISAMP, IUNIT, PRSEC
       INTEGER    I, LC, LD, LR, LT, LU
C
       REAL*8     RA, DEC
C
       include '/mrao/include/constants.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/control_tab_v0.inc'
C
       IF (STATUS.NE.0) RETURN
C
       CALL io_enqout(IUNIT)
C
C  Print observation title
C
       WRITE(IUNIT,*)
       IF (OPTION.EQ.'TITLE') THEN
         IPRINT=1
         LTIME='GMT'
         IDATE(1)=IDAT1(1)
         IDATE(2)=IDAT1(2)
         IDATE(3)=IDAT1(3)
         CALL chr_chdate(IDATE,IPRINT,CDATE,LD)
         IF (IUTIM.EQ.1) LTIME='BST'
         WRITE(IUNIT,1)TITLE,ITIM1(3),ITIM1(2),LTIME,CDATE
         WRITE(IUNIT,2)NSP,NSAMP,FROBS/1.E6
         LC=chr_lenb(CTEXT(1:80))
         IF (LC.GT.0) WRITE(IUNIT,'(2X,A)')CTEXT(1:LC)
    1    FORMAT(2X,A16,'started',2X,I2,'.',I2.2,X,A,2X,A)
    2    FORMAT('  File contains',I5,' spacings and',I6,' samples'/
     :          '  Observing frequency',F6.1,' MHz')
C
C  Print observation parameters
C
       ELSEIF (OPTION.EQ.'PARAMETERS') THEN
C
         LT=chr_lenb(TITLE)
         LU=chr_lenb(AUNITS)
c        ISAMP=2*INTSAM*ISAMPS/10
         ISAMP=2*INTSAM*(ITAB(1)+ITAB(2))/10000
         CALL chr_chucas(TITLE(1:LT))
         WRITE(IUNIT,'(2X,A,''-'',3I2.2)')
     :     TITLE(1:LT),IDAT1(1),IDAT1(2),MOD(IDAT1(3),1900)
         WRITE(IUNIT,'(2X,A,4X,I6)')
     :    'Number of samples   :',NSAMP
         WRITE(IUNIT,'(2X,A,4X,I6)')
     :    'Number of spacings  :',NSP
         WRITE(IUNIT,'(2X,A,3X,F7.1,A)')
     :    'Observing frequency :',FROBS/1.E6,' MHz'
         WRITE(IUNIT,'(2X,A,6X,I4,A)')
     :    'Integration time    :',ISAMP,' secs'
         WRITE(IUNIT,'(2X,A,3X,F7.1,X,A)')
     :    'Amplitude cut-off   :',ACHOP*AMPSCL,AUNITS(1:LU)
         IF (IAMPF.EQ.0) WRITE(IUNIT,*)' Amplitude factors suppressed'
C
C    Print map centre (phase centre)
C
         PRSEC=1
         IF (RAMC.GE.0.) THEN
           CALL chr_chdtos(RAREF/CONST_H2R,PRSEC,CHRA,LR)
           CALL chr_chdtos(DECREF/CONST_D2R,PRSEC,CHDEC,LD)
           WRITE(IUNIT,'(/2X,A,F7.2,2A,X,A)')
     :     'Map centre (',DATREF,')  ',CHRA(1:LR),CHDEC(1:LD)
           CALL chr_chdtos(RAMC/CONST_H2R,PRSEC,CHRA,LR)
           CALL chr_chdtos(DECMC/CONST_D2R,PRSEC,CHDEC,LD)
           WRITE(IUNIT,'(2X,A,F7.2,2A,X,A)')
     :     '           (',DATOBS,')  ',CHRA(1:LR),CHDEC(1:LD)
         ELSE
           CALL chr_chdtos(RAREF/CONST_H2R,PRSEC,CHRA,LR)
           CALL chr_chdtos(DECREF/CONST_D2R,PRSEC,CHDEC,LD)
           WRITE(IUNIT,3)CHRA(1:LR),CHDEC(1:LD)
    3      FORMAT(/2X,'Map centre ',11('.'),A,X,A)
         ENDIF
C
C    Print pointing centre
C
         IF (ISETAE.EQ.1) THEN
           RA=ABS(RAAE)
           DEC=DECAE
           CALL chr_chdtos(RA/CONST_H2R,PRSEC,CHRA,LR)
           CALL chr_chdtos(DEC/CONST_D2R,PRSEC,CHDEC,LD)
           IF (RAAE.GE.0.) WRITE(IUNIT,4)CHRA(1:LR),CHDEC(1:LD)
           IF (RAAE.LT.0.) WRITE(IUNIT,4)CHRA(1:LR),CHDEC(1:LD),
     :       'not tracking'
    4      FORMAT(2X,'Aerials ',14('.'),A,X,A,2X,A)
         ENDIF
C
C    Print centre for path compensation
C
         IF (ISETPC.EQ.1) THEN
           RA=ABS(RAPC)
           DEC=DECPC
           CALL chr_chdtos(RA/CONST_H2R,PRSEC,CHRA,LR)
           CALL chr_chdtos(DEC/CONST_D2R,PRSEC,CHDEC,LD)
           IF (RAPC.GE.0.) WRITE(IUNIT,5)CHRA(1:LR),CHDEC(1:LD)
           IF (RAPC.LT.0.) WRITE(IUNIT,5)CHRA(1:LR),CHDEC(1:LD),
     :       'not tracking'
    5      FORMAT(2X,'PCs ',18('.'),A,X,A,2X,A)
         ENDIF
C
C   Print observation start and stop times
C
         LTIME='GMT'
         IF (IUTIM.EQ.1) LTIME='BST'
         WRITE(STRING,6)ISTIM1(3),ISTIM1(2),ISTIM1(1),
     :     ITIM1(3),ITIM1(2),LTIME
         WRITE(IUNIT,'(2X,A)')STRING
         WRITE(STRING,6)ISTIM2(3),ISTIM2(2),ISTIM2(1),
     :     ITIM2(3),ITIM2(2),LTIME
         STRING(1:11)='Stop time .'
         WRITE(IUNIT,'(2X,A)')STRING
    6    FORMAT('Start time ',11('.'),2X,2(I2.2,'.'),I2.2,' ST',
     :     3X,I2.2,'.',I2.2,X,A)
C
C    Print sources used for real-time calibration or ionospheric
C    correction.
C
         IF (NCAL.GT.0) THEN
           IF (IONSPH.EQ.0) WRITE(IUNIT,7)
           IF (IONSPH.NE.0) WRITE(IUNIT,8)
           DO I=1,NCAL
             LC=chr_lenb(CSOURCE(I))
             CALL chr_chdtos(CAL(1,I)/CONST_H2R,PRSEC,CHRA,LR)
             CALL chr_chdtos(CAL(2,I)/CONST_D2R,PRSEC,CHDEC,LD)
             WRITE(STRING,9)CHRA(1:LR),CHDEC(1:LD)
             IF (LC.GT.0) STRING(1:LC+1)=CSOURCE(I)(1:LC)//' '
             WRITE(IUNIT,'(2X,A)')STRING
           ENDDO
    7      FORMAT(/2X,'Real time calibration source')
    8      FORMAT(/2X,'Ionospheric correction made with')
    9      FORMAT(22('.'),A,X,A)
         ENDIF
C
         WRITE(IUNIT,*)
         WRITE(IUNIT,'(2X,A)')'Sample file '//SFILE(1:LSF)
         IF (NCAL.GT.0 .AND. IONSPH.EQ.0) THEN
           WRITE(IUNIT,'(2X,A)')'Calibration file '//CFILE(1:LCF)
         ENDIF
         IF (IZF.GT.0) THEN
           WRITE(IUNIT,'(2X,A)')'Zero-correction file '//ZFILE(1:LZF)
         ELSE
           WRITE(IUNIT,*)' NO zero-correction file'
         ENDIF
C
         IF (ISETCYC.EQ.1) THEN
           WRITE(STRING,10)INTCYC
           IF (KCYCLE.EQ.1) STRING(1:5)='EXCYC'
           IF (KCYCLE.EQ.2) STRING(1:5)='NODDY'
           IF (KCYCLE.EQ.3) STRING(1:5)='PCYC '
           WRITE(IUNIT,*)
           WRITE(IUNIT,'(2X,A)')STRING(1:40)
   10     FORMAT(5X,' switched on, running every',I3,' mins')
         ENDIF
       ENDIF
C
       IF (OPTION.EQ.'PARAMETERS') WRITE(IUNIT,*)
       WRITE(IUNIT,*)
C
       END
