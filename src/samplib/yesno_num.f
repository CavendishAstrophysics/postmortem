


*$(8)  Miscellaneous routines

*+yesno_NUM

       LOGICAL FUNCTION yesno_NUM (PROMPT, DEFAULT, NUMBER, STATUS)
C      ------------------------------------------------------------
C
C  A combination of 'io_yesno' and io_geti for use in searching files.
C
C  Given:
C      PROMPT    char*(*)    prompt string
C      DEFAULT   char*(*)    default input string
C
C  Returned:
C      NUMBER    integer     User input integer
C      STATUS    integer     status value
C
C  Reads the next word from the command line, prompting for more input
C  if necessary, and returns the value .true. if the input string matches
C  'YES', or .false. if 'NO' or a valid integer. If the input string is
C  a valid integer the parameter NUMBER is returned with this value,
C  otherwise it is unchanged. Upper and lower case input are both
C  recognised.  If the input string is null, the DEFAULT string is
C  taken as input.  If the default string is '*', the value .false.
C  is returned by default.  If the default string is null, the routine
C  will insist on a response of 'YES' or 'NO' before returning.
C
C  The STATUS value should be zero on entry.  The returned status will
C  be zero unless user break is recognised, in which case the routine
C  will return the value .false.
C
*-
       CHARACTER*(*) PROMPT, DEFAULT
       CHARACTER*3   RESPONSE
       INTEGER       LENGTH, NUMBER, STATUS, TEMP
       LOGICAL       chr_match
C
       include '/mrao/include/iolib_errors.inc'
C
       CHARACTER*32  ERRMSG
       PARAMETER   ( ERRMSG = 'enter yes/no or an integer value.' )
C
       yesno_NUM=.FALSE.
C
    1  IF (STATUS.EQ.0) THEN
         RESPONSE='no'
         IF (DEFAULT.EQ.' ') RESPONSE='*'
         CALL io_getwrd(PROMPT,DEFAULT,RESPONSE,LENGTH,STATUS)
         IF (STATUS.EQ.0) THEN
C
           CALL chr_chucas(RESPONSE(1:LENGTH))

           IF (chr_match(RESPONSE(1:LENGTH),'YES')) THEN
             yesno_NUM=.TRUE.
           ELSEIF (chr_match(RESPONSE(1:LENGTH),'NO')) THEN
             yesno_NUM=.FALSE.
           ELSE
             CALL chr_chctoi(RESPONSE(1:LENGTH),TEMP,STATUS)
             IF (STATUS .NE. 0) THEN
               STATUS=BAD_SYNTAX
               CALL io_wrmsg(STATUS,ERRMSG)
               CALL io_setcli(' ')
               GOTO 1
             ELSE
               NUMBER = TEMP
             ENDIF
           ENDIF
C
         ENDIF
       ENDIF
C
       END
