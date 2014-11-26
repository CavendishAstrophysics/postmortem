C+
       SUBROUTINE EX_SAMP (PARAMETERS, STATUS)
C      ---------------------------------------
C
C  Executes the EXAMINE-SAMP utility program.
C
C  Given:
C      PARAMETERS  char*(*)    input parameter string
C
C  Returned:
C      STATUS      integer     status value
C
C  Program to examine sample files on disc and magnetic tape archive.
C  This routine decodes the input parameters string, prompting for more
C  input if necessary, and calls the appropriate EXAMINE-SAMP routine.
C
C  The STATUS value should be zero on entry.
C
C-
       CHARACTER PARAMETERS*(*)
       INTEGER  STATUS, NOPT
C
       PARAMETER  (NOPT=6)
       CHARACTER  OPTIONS(NOPT)*60, OPT*12
       CHARACTER  DEF_DIR*32, FILE*64, STRING*64
c      CHARACTER  VOLUME*24
c      INTEGER    I1, I2, LV
       INTEGER    IOLD, IOUT, TERMI, TERMO, LF
       LOGICAL    FLAG, INTER
C
       include '/mrao/include/iolib_errors.inc'
       include '/mrao/include/iolib_functions.inc'
       include '/mrao/include/chrlib_functions.inc'
C
       DATA  OPTIONS/
     * ' LIST .......... list sample filenames (default)',
     * ' TITLE ......... print observation title',
     * ' PARAMETERS .... print observation parameters',
c    * ' SAVE .......... mark sample file for saving on tape',
     * ' DELETE ........ delete sample file from disc',
c    * ' CLEAR ......... clear saved sample files from disc',
c    * ' TAPE .......... find sample file in tape archive',
     * ' INTERACTIVE ... enter interactive mode',
     * ' QUIT .......... exit from EXAMINE-SAMP'/
C
       IF (STATUS.NE.0) RETURN
C
       CALL IO_ENQOUT(IOUT)
       IOLD=IOUT
C
C  Prime command line with parameter string
C
       CALL IO_SETCLI(PARAMETERS)
C
C  Get sample file pattern, default directory given by environment variable
C
       CALL GETENV('SAMPDIR',DEF_DIR)
       CALL IO_GETC('sample file: ',' ',STRING,STATUS)
       CALL IO_MAKFIL(DEF_DIR,STRING,' ',FILE,LF)
C
C  Get option, default is 'LIST'
C
       OPT='LIST'
       CALL IO_GETOPT('option: ',' ',OPTIONS,NOPT,OPT,STATUS)
       IF (OPT.EQ.'QUIT') STATUS=USR_BREAK
C
C  Get tape volume listing file
C
c      IF (OPT.EQ.'TAPE') THEN
c        FILE=STRING
c        STRING='SAM'
c        I1=INDEX(USER,':')+1
CC       IF (match(USER(I1:),'RYLE')) STRING='R'
c        CALL io_getc('tape volume: ',' ',STRING,STATUS)
c        CALL io_makfil(USER(I1:),STRING,'VOL',VOLUME,LV)
c        CALL chr_chucas(VOLUME(1:LV))
c      ENDIF
C
       INTER=OPT.EQ.'INTERACTIVE'
C
C  Offer interactive mode for DELETE options
C
       IF (OPT.EQ.'DELETE' .OR. OPT.EQ.'CLEAR') THEN
         CALL IO_SETCLI(' ')
         IF 
     :   (IO_YESNO('manual check for delete option?','yes',STATUS)) THEN
           INTER=.TRUE.
         ENDIF
C
       ELSEIF (OPT.NE.'INTERACTIVE') THEN
C
C  Get output file, write header text if not to terminal
C
         CALL IO_OPEOUT(IOUT,STATUS)
         CALL IO_ENQTIO(TERMI,TERMO)
         IF (STATUS.EQ.0) THEN
           IF (IOUT.NE.TERMO) THEN
             LF=CHR_LENB(FILE)
             STRING='EXAMINE-SAMP '//FILE(1:LF)
             CALL IO_LOGOUT(STRING(1:LF+13),1)
           ENDIF
         ENDIF
       ENDIF
C
       CALL IO_ENQILF(FLAG)
       IF (FLAG .OR. INTER) WRITE(*,*)
       IF (INTER) CALL io_setcli(' ')
C
C  Call the appropriate subroutine
C
c      IF (OPT.EQ.'TAPE') THEN
c        CALL EXS_TAPE(FILE,VOLUME,STATUS)
c      ELSE
         CALL EXS_DISC(FILE,OPT,INTER,STATUS)
c      ENDIF
C
       CALL IO_CLOSE(IOUT,STATUS)
       CALL IO_SETOUT(IOLD)
       CALL IO_WROUT(' ')
C
       STATUS=0
C
       END
