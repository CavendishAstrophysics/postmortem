C+
       SUBROUTINE EXS_DISC (FILE, OPTION, INTER, STATUS)
C      -------------------------------------------------
C
C  Executes EXAMINE-SAMP functions for sample files on disc.
C
C  Performs the specified EXAMINE-SAMP option.  The INTER control flag
C  specifies whether files are examined interactively or not.
C
C  The STATUS value should be zero on entry.
C
C-
       CHARACTER*(*) FILE, OPTION
       INTEGER       STATUS
C
       INTEGER    NFILES
       PARAMETER (NFILES=256)
       CHARACTER  FILES(NFILES)*64, LFILE*64, NFILE*64
C
       INTEGER    NOPT
       PARAMETER  (NOPT=5)
       CHARACTER  OPTIONS(NOPT)*60, OPT*12
       CHARACTER  STRING*80, FNAME*64, DEF_DIR*32
       CHARACTER  CDATE*16, RDATE*16, SAVED*16, CFILE*6
       INTEGER*4  BUFFER(13), ITIME(9), IDATE, IWEEK, ICR, IRD, IWR
       INTEGER    LD, LF, LR, LS, MF, MSAVE, NF, NBYTE, NR
       INTEGER    ILIST, I, I1, I2, IN, IOBJX, J
       INTEGER    KEY(2), NKEY, LSTATUS
       LOGICAL    INTER
C
       include '/mrao/include/iolib_errors.inc'
       include '/mrao/include/iolib_functions.inc'
       include '/mrao/include/chrlib_functions.inc'
C
       DATA OPTIONS/
     * ' TITLE ......... print observation title',
     * ' PARAMETERS .... print observation parameters',
c    * ' SAVE .......... mark sample file for saving on tape',
     * ' DELETE ........ delete sample file from disc',
     * ' NEXT .......... find next sample file on disc',
     * ' QUIT .......... exit from EXAMINE-SAMP'/
C
       IF (STATUS.NE.0) RETURN
C
       CALL io_enqout(ILIST)
C
C  Set up selection criterion for 'CLEAR' option, selects all saved
C  files not used within the past week.
C
       IF (OPTION.EQ.'CLEAR') THEN
         DO I=1,6
           ITIME(I)=0
         ENDDO
         ITIME(4)=7
         CALL util_datint(ITIME,IWEEK)
         CALL util_enqtim(ITIME)
         CALL util_enqdat(ITIME(4))
         CALL util_datint(ITIME,IDATE)
         IDATE=IDATE-IWEEK
         WRITE(ILIST,'(X,A)')
     :     'Delete saved sample files not used within the past week'
       ENDIF
C
C  Examine all files matching FILE
C
    1  MF=0
       NF=0
       NR=0
C
       LS=chr_ilstc(FILE,'/')
       IF (FILE(LS:).EQ.'/s5'   .OR.
     :     FILE(LS:).EQ.'/s15'  .OR.
     :     FILE(LS:).EQ.'/s27'  .OR.
     :     FILE(LS:).EQ.'/s31'  .OR.
     :     FILE(LS:).EQ.'/samp' .OR.
     :     FILE(LS:).EQ.'/cal'  .OR.
     :     FILE(LS:).EQ.'/rem') THEN
         LFILE=FILE
       ELSE
         LS=chr_lenb(FILE)
         LFILE=FILE(1:LS)//'*/s*'
       ENDIF
C
       DO WHILE (STATUS.EQ.0)
    2    CALL io_nxtfil(LFILE,NFILE,IOBJX,NF,STATUS)
         IF (STATUS.EQ.0) THEN
           LS=chr_ilstc(NFILE,'/')
           IF (NFILE(LS:).NE.'/s5'   .AND.
     :         NFILE(LS:).NE.'/s15'  .AND.
     :         NFILE(LS:).NE.'/s27'  .AND.
     :         NFILE(LS:).NE.'/s31'  .AND.
     :         NFILE(LS:).NE.'/samp' .AND.
     :         NFILE(LS:).NE.'/cal'  .AND.
     :         NFILE(LS:).NE.'/rem') THEN
             LSTATUS=0
             STRING=NFILE(1:chr_lenb(NFILE))//'/s*'
             CALL IO_NAMFIL(STRING,NFILE,0,LSTATUS)
             IF (LSTATUS.NE.0) GOTO 2
           ENDIF
         ENDIF
C
C    If last file, sort the files and process the list
C
         IF (STATUS.NE.0) THEN
           LSTATUS=STATUS
           STATUS=0
C
           NKEY=1
           KEY(1)=1
           KEY(2)=64
c          CALL util_qsortc(FILES,NR,KEY,NKEY)
C
           DO I=1,NR
C
             FNAME=FILES(I)
             LF=chr_lenb(FNAME)
             I1=chr_ilstc(FNAME,'/')-1
             I2=chr_ilstc(FNAME(1:I1),'/')-1
             DEF_DIR=FNAME(1:I2)
             IN=I2+2
C
C    Read object entry and control tables
C
             CALL EXS_READ(FNAME,BUFFER,MSAVE,STATUS)
             IF (STATUS.EQ.0) THEN
               DO J=9,11
                 CALL LTIME(BUFFER(J),ITIME)
                 ITIME(5)=ITIME(5)+1
                 ITIME(6)=ITIME(6)+1900
                 CALL util_datint(ITIME,BUFFER(J))
               ENDDO
             ELSE
               STATUS=0
               GOTO 4
             ENDIF
C
C      Apply selection criterion for the CLEAR option
C
             IF (OPTION.EQ.'CLEAR') THEN
               IRD=IDATE-BUFFER(9)
               IWR=IDATE-BUFFER(10)
               ICR=IDATE-BUFFER(11)
               IF (MSAVE.NE.2 .OR.
     :             (IRD.LT.0 .AND. BUFFER(9).NE.0) .OR.
     :             (IWR.LT.0 .AND. BUFFER(10).NE.0)) GOTO 4
             ENDIF
C
C      Set up output text and print
C
             IF (STATUS.EQ.0) THEN
               NBYTE=BUFFER(8)
               MF=MF+1
C
               IF (MSAVE.EQ.0) THEN
                 SAVED='not saved'
               ELSEIF (MSAVE.EQ.1) THEN
                 SAVED='save requested'
               ELSEIF (MSAVE.EQ.2) THEN
                 SAVED='saved on tape'
               ELSEIF (MSAVE.EQ.3) THEN
                 SAVED='modified'
               ENDIF
               CALL util_extdat(BUFFER(9),1,RDATE,LR)
               CALL util_extdat(BUFFER(11),1,CDATE,LD)
C
C        Set up header line
C
               IF (I.EQ.1) THEN
                 IF (MF.GT.1) WRITE(ILIST,*)
                 WRITE(ILIST,'(X,A,A/)') 'Directory ',DEF_DIR
               ENDIF
               IF (OPTION.EQ.'LIST') THEN
                 IF (I.EQ.1) THEN
                   WRITE(ILIST,'(29X,A,6X,A,4X,A)')
     :                                 'bytes','created','last used'
                 ENDIF
                 WRITE(ILIST,'(2X,A24,I8,3(4X,A))')
     :                  FNAME(IN:),NBYTE,CDATE(1:9),RDATE(1:9),SAVED
C
               ELSEIF (.NOT.INTER) THEN
                 IF (OPTION.EQ.'DELETE' .OR. OPTION.EQ.'CLEAR') THEN
                   WRITE(ILIST,'(X,A32,I8,2A)')
     :               FNAME(IN:),NBYTE,' bytes, last used ',RDATE(1:9)
                 ELSE
                   WRITE(ILIST,'(X,A32,I8,2A)')
     :                 FNAME(IN:),NBYTE,' bytes, created ',CDATE(1:9)
                 ENDIF
C
               ELSE
                 IF (OPTION.EQ.'DELETE' .OR. OPTION.EQ.'CLEAR') THEN
                   WRITE(STRING,'(A32,I8,3A)')
     :             FNAME(IN:),NBYTE,' bytes, last used ',RDATE(1:6)
                 ELSE
                   WRITE(STRING,'(A32,I8,2A)')
     :             FNAME(IN:),NBYTE,' bytes, ',SAVED
                 ENDIF
                 LS=chr_lenb(STRING)
                 STRING(LS+1:)=' : '
                 LS=LS+2
               ENDIF
C
C      Interactive mode, prompt for option
C
    3          IF (INTER) THEN
                 OPT='NEXT'
                 CALL io_getopt(STRING(1:LS),' ',OPTIONS,NOPT,OPT,
     :                                                        STATUS)
               ELSE
                 OPT=OPTION
               ENDIF
C
C      Perform required function
C
               IF (OPT.EQ.'TITLE') THEN
                 CALL EXS_PRINT('TITLE',STATUS)
                 IF (INTER) GOTO 3
               ELSEIF (OPT.EQ.'PARAMETERS') THEN
                 CALL EXS_PRINT('PARAMETERS',STATUS)
                 IF (INTER) GOTO 3
               ELSEIF (OPT.EQ.'SAVE') THEN
c                CALL EXS_SAVE(FNAME,MSAVE,INTER,STATUS)
               ELSEIF (OPT.EQ.'DELETE' .OR. OPT.EQ.'CLEAR') THEN
                 CALL EXS_DELETE(FNAME,MSAVE,INTER,STATUS)
               ELSEIF (OPT.EQ.'QUIT') THEN
                 STATUS=USR_BREAK
               ENDIF
C
               IF (STATUS.EQ.USR_BREAK) GOTO 5
               IF (io_attn(STATUS)) GOTO 5
C
             ENDIF
    4        CONTINUE
           ENDDO
C
           IF (STATUS.EQ.0) STATUS=LSTATUS
           NR=0
C
         ENDIF
C
         NR=NR+1
         FILES(NR)=NFILE
C
       ENDDO
C
    5  IF (STATUS.GT.0) THEN
         IF (MF.GT.0) THEN
           IF (OPTION.EQ.'LIST') THEN
             CALL chr_chitoc(MF,CFILE,LF)
             WRITE(ILIST,'(/X,5A)')
     :        'Total of ',CFILE(1:LF),' files'
           ENDIF
         ELSEIF (OPTION.EQ.'DELETE' .OR. OPTION.EQ.'CLEAR') THEN
           WRITE(ILIST,*)'*** no files deleted'
         ELSE
           LF=chr_lenb(FILE)
           WRITE(ILIST,*)'*** no file found matching ',FILE(1:LF)
         ENDIF
       ENDIF
C
       IF (OPTION.EQ.'INTERACTIVE') THEN
         STATUS=0
         STRING=' '
         WRITE(ILIST,*)
         CALL io_getc('sample file:',' ',STRING,STATUS)
         CALL io_makfil(DEF_DIR,STRING,' ',FILE,LF)
         IF (STRING.NE.' ') THEN
           WRITE(ILIST,*)
           GOTO 1
         ENDIF
       ENDIF
C
       END
