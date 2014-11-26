C+ENQ_FULL_NAME

       subroutine enq_full_name ( abbr_name, full_name, amb_flag, s )
C
C     Returns the expanded file name for a sample file.
C
C     Given:
C         Input sample file name abbreviation.
              character*(*)   abbr_name
C         Flag set if ambiguous file names are to be resolved.
              logical         amb_flag

C     Returned:
C         Full file name in the form path/name.type.
              character*(*)   full_name
C         Status variable - must be zero on entry - otherwise error
              integer         s
C
C     The first time this routine is called it builds up a list of
C     valid sample file names in the default directories.
C
C     Thereafter, if input sample file name does not have a specified
C     directory:user field, then it is matched with this list, otherwise
C     the iolib routine io_namfil is executed.
C
C     If the job is running interactively, and the amb_flag is set true,
C     then if the initial filename is ambiguous the user is asked to
C     select from the possible alternatives. If they do not select any
C     a status of NO_FILE is returned.
C
C     Possible return status's are:
C         TOO_MANYSF  - Too many sample files in default directories.
C         AMB_FILE    - File name is ambiguous. (error not logged)
C         NO_FILE     - File does not exist. (error not logged)
C         Other       - Unexpected system error.
C
C     (NPR, 16 December 1987)
C     (DJT, 19 February 1990)
C     (DJT, 29 June 1992, Unix implementation)
C
C-
C     ****************************************************************
C
C     Function declarations
C
      include '/mrao/include/chrlib_functions.inc'
      include '/mrao/include/iolib_functions.inc'

C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include '/mrao/include/iolib_errors.inc'
      include '/mrao/post/include/samplib_errors.inc'

C     ****************************************************************
C
C     Local constant and variable declarations
C         Maximum number of sample files allowed in default directories.
              integer         max_sf
              parameter     ( max_sf = 256 )
C         Default sample file directory and file type patterns
              character*16    def_dir
              character*4     def_type
              parameter     ( def_dir = '/data4/*/' )
              parameter     ( def_type = 'S*' )
C
C     Variables, equivalences and commons
C         Loop counter and string length
              integer         i, l1, ls
C         Batch process flag
              logical         batch
C         Default sample file name
              character*64    def_name
C         Current file name
              character*64    file_name
C         Number of files matching given name.
              integer         num_match
C         Pointer to file which last matched.
              integer         last_match
C         Object index and count for io_nxtfil
              integer         objx, count
C         Number of sample files in default directories.
              integer         num_sf
C         List of sample file names.
              character*64    sf_list(max_sf)

          common  / sf_list / num_sf, sf_list
          data      num_sf  / 0 /

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

      call io_enqbch( batch )

C     ****************************************************************
C
C         Main Code
C         ---------
C
      l1 = index (abbr_name, '/')
      if ( l1.gt.0 ) then
C         Abbreviated name has a directory component
          call io_makfil( ' ', abbr_name, def_type, file_name, i )
          call io_namfil( file_name, full_name, 0, s )

          if (s.eq.AMB_FILE .and. amb_flag .and. .not.batch) then
C             Resolve ambiguous file reference
              count = 0
              s = 0

  100         continue
                  call io_nxtfil( abbr_name, file_name, objx, count, s )
                  if (s .eq. 0) ls = chr_lenb(file_name)
           if (s.eq.0 .and. .not.io_yesno(file_name(1:ls)//'?','no',s ))
     *                                                          goto 100
              full_name = file_name(1:ls)
          end if
          if ( s .ne. 0 ) full_name = ' '
      else
C         No directory - check to see if sample file list exists.
          if (num_sf .eq. 0) then
              num_sf = 0
              count  = 0

  200         continue
                  call io_makfil( def_dir, '*', def_type, def_name, i )
                  call io_nxtfil( def_name, file_name, objx, count,s)
                  if (s .eq. 0 .and. num_sf.lt.max_sf) then
                      num_sf = num_sf+1
                      i = chr_lenb(file_name)
                      sf_list(num_sf) = file_name(1:i)
                  else if (s .eq. 0) then
                      s = TOO_MANYSF
                  end if
              if (s .eq. 0) goto 200

C             Check for normal termination of io_nxtfil
              if (s .eq. NO_FILE) s = 0
          end if
          if (s .ne. 0) goto 999

C         Now check to see if file is in list
          num_match  = 0
          last_match = 0
          call io_makfil( ' ', abbr_name, def_type, file_name, i )

          do 300, i = 1, num_sf
              if (file_name.eq.sf_list(i)) then
                  num_match = 1
                  last_match = i
                  goto 301
              else if (chr_fmatch( file_name, sf_list(i) )) then
                  num_match  = num_match+1
                  last_match = i
              end if
  300     continue
  301     continue

          if (num_match .gt. 1 .and. amb_flag .and. .not.batch) then
C             Resolve ambiguous file reference
              num_match = 0
              i = 0
  400         continue
                  i = i+1
                  if (chr_fmatch( file_name, sf_list(i) )) then
                      ls = chr_lenb(sf_list(i))
                    if (io_yesno( sf_list(i)(1:ls)//'?', 'no', s )) then
                          num_match  = 1
                          last_match = i
                      end if
                  end if
              if (i.lt.num_sf .and. num_match.ne.1 .and.s.eq.0) goto 400
          end if

          if ( s .eq. USR_BREAK ) then
              continue
          else if (num_match .eq. 0) then
              s = NO_FILE
              full_name = ' '
          else if (num_match .eq. 1) then
              s = 0
              full_name = sf_list(last_match)
          else
              s = AMB_FILE
              full_name = ' '
          end if
      end if

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 999  call smp_wrerr( s, 'in subroutine ENQ_FULL_NAME' )

      end
