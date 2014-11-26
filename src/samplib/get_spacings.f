

C+GET_SPACINGS

       subroutine get_spacings ( lun, prompt, default, list, ilist,
     :                                            mspac, nspac, status )
C
C  Reads a spacing list from the command line.
C
C  Given:
C      LUN       integer     sample file unit number
C      PROMPT    char*(*)    prompt string
C      DEFAULT   char*(*)    default input string
C      MSPAC     integer     maximum number of spacings in the list
C
C  Returned:
C      LIST      char*(*)    string containing input text
C      ILIST     integer(*)  array containing spacing index numbers
C      NSPAC     integer     number of spacings in the list
C      STATUS    integer     status value
C
C  Routine to read a spacing list from the command line, and return an
C  array containing the index numbers of the spacings included in the
C  list, in increasing order.  Various formats are recognised:
C
C      -  'AE 1,3,5-12,14,...'   specifies spacings by aerial number
C      -  'HUTS A,C,...'         specifies spacings by hut identifier
C      -  'ALL'                  specifies all available spacings
C
C  The input text may consist of an East and West aerial list, separated
C  by the '/' delimiter : in this case all spacings obtained by switching
C  East/West aerial pairs will be included.
C
C  The status value should be zero on entry.  The returned status value is
C  zero, unless bad syntax or a user break has been detected.
C
*-
       include '/mrao/include/chrlib_functions.inc'
C
       CHARACTER*(*)  PROMPT, DEFAULT, LIST
       INTEGER  LUN, MSPAC, ILIST(MSPAC), NSPAC, STATUS
       INTEGER  LENGTH
       LOGICAL  BATCH
C
    1  IF (STATUS.EQ.0) THEN
C
C    Check command line -- if string is quoted read one word if not
C    read the whole command line as input string
C
         CALL io_enqcli( LIST, LENGTH )
         IF (LIST(1:1).EQ.'''') THEN
           CALL io_getwrd(PROMPT,DEFAULT,LIST,LENGTH,STATUS)
         ELSE
           CALL io_getstr(PROMPT,DEFAULT,LIST,STATUS)
           LENGTH = chr_lenb(LIST)
         END IF
C
C    Check for validity, resolve the list into index numbers
C
         IF (LENGTH.GT.0) THEN
           CALL SET_SPACINGS(LUN,LIST(1:LENGTH),ILIST,MSPAC,NSPAC,
     *                                                           STATUS)
           CALL io_enqbch(BATCH)
           IF (STATUS.NE.0) THEN
             CALL io_wrout('*** illegal spacing list')
             IF (.NOT.BATCH) STATUS=0
             GOTO 1
           ELSEIF (NSPAC.EQ.0) THEN
             CALL io_wrout('*** these spacings not present')
             IF (.NOT.BATCH) GOTO 1
           ENDIF
         ELSE
           NSPAC=0
         ENDIF
       ENDIF
C
       END
