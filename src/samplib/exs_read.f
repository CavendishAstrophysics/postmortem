C+
       SUBROUTINE EXS_READ (FILE, BUFFER, MSAVE, STATUS)
C      -------------------------------------------------
C
C  Routine to read the object entry and control tables from a sample
C  file on disc.  If the user has directory access to the file, the
C  object entry will be preserved during the process of reading the
C  control tables (i.e. opening the file is not recorded).
C
C  MSAVE reports the SAVE status of the file:
C
C     MSAVE = 0  sample file not saved on tape.
C     MSAVE = 1  sample file marked for saving (save requested)
C     MSAVE = 2  sample file saved on tape.
C     MSAVE = 3  sample file modified since last saved.
C
C  The STATUS value should be zero on entry.
C
C-
       CHARACTER  FILE*(*)
       INTEGER*4  BUFFER(13)
       INTEGER    MSAVE, SAVE_FLAG, STATUS
C
       CHARACTER  FNAME*64, LFILE*24
       INTEGER    I1, I2, IFILE, LF
C
       INTEGER*4  ISTR(20)
       CHARACTER  STRING*80
       EQUIVALENCE (ISTR,STRING)
C
       include '/mrao/post/include/control_tables.inc'
       include '/mrao/include/chrlib_functions.inc'
C
       IF (STATUS.NE.0) RETURN
C
C  Find the file indexes and read the object entry
C
       MSAVE=0
       LF=chr_lenb(FILE)
       CALL io_rdobjn(FILE,BUFFER,STATUS)
C
C    Open the sample file and read the control tables
C
       CALL io_nxtlun(IFILE,STATUS)
       OPEN (IFILE, FILE=FILE, FORM='UNFORMATTED', RECL=8192,
     :              ACCESS='DIRECT', STATUS='OLD', IOSTAT=STATUS)
       IF (STATUS.EQ.0) THEN
         CALL READ_CT(IFILE,STATUS)
         CALL ENQ_SAVE_FLAG(IFILE,SAVE_FLAG,STATUS)
         CLOSE(IFILE)
         CT_LUN = 0
       ENDIF
C
C      Check whether file is still on the save list
C
       IF (STATUS.EQ.0) THEN
         IF (SAVE_FLAG.EQ.2) THEN
           MSAVE=2
           CALL io_namfil(FILE,FNAME,0,STATUS)
           IF (STATUS.EQ.0) THEN
             I2=INDEX(FNAME,')')
             I1=INDEX(FNAME(1:I2),':')+1
             LFILE='('//FNAME(I1:I2)//'SAVE-SAMP:LIST'
             OPEN(IFILE, FILE=LFILE, ACCESS='SEQUENTIAL', 
     :                            STATUS='OLD', IOSTAT=STATUS)
             IF (STATUS.EQ.0) THEN
               DO WHILE (STATUS.EQ.0)
                 READ(IFILE,'(A)',END=1)STRING
                 IF (chr_fmatch(STRING,FNAME)) THEN
                   STATUS=-1
                   MSAVE=1
                 ENDIF
               ENDDO
    1          CLOSE(IFILE)
             ENDIF
             STATUS=0
           ENDIF
C
C   Check whether file is marked as modified
C
         ELSEIF (SAVE_FLAG.EQ.3) THEN
           MSAVE=3
         ENDIF
C
       ENDIF
C
       IF (STATUS.NE.0) CALL SMP_WRERR(STATUS,FILE)
C
       END
