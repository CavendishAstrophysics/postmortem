C+ENQ_SFNAME

       subroutine enq_sfname ( abbr_name, full_name, amb_flag, s )
C
C     Returns the complete file name for a sample file.
C
C     Given:
C         Input sample file name abbreviation.
              character*(*)   abbr_name
C         Flag set if ambiguous file names are to be resolved.
              logical         amb_flag

C     Returned:
C         Full file name in the form path/name/type.
              character*(*)   full_name
C         Status variable - must be zero on entry - otherwise error
              integer         s
C
C     If the job is running interactively, and amb_flag is set true,
C     then if the initial filename is ambiguous the user is asked to
C     select from the possible alternatives. If none is selected a
C     status of NO_FILE is returned.
C
C     Possible return status's are:
C         TOO_MANYSF  - Too many sample files in default directories.
C         AMB_FILE    - File name is ambiguous.
C         NO_FILE     - File does not exist.
C         Other       - Unexpected system error.
C
C     (NPR, 16 December 1987, as ENQ_FULL_NAME)
C     (DJT, 19 February 1990)
C     (DJT, 18 August 1993, Unix implementation, as ENQ_SFNAME)
C     (GGP,  1 June 99, removed search through matching files)
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
      include '/mrao/post/include/post_common.inc'
      include '/mrao/post/include/samplib_errors.inc'

C     ****************************************************************
C
C     Local constant and variable declarations
C         Maximum number of sample files considered in current directory
              integer         max_sf
              parameter     ( max_sf = 128 )
C
C     Variables, equivalences and commons
C         Loop counter, string index
              integer         i, ls
C         Batch process flag
              logical         batch
C         Number of files matching given name
              integer         num_match
C         Temporary file name
              character*64    temp_name
C         Current file name
              character*64    file_name
C         List of sample file names
              character*64    sf_list(max_sf)
C         Object index and count for io_nxtfil
              integer         objx, count

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

C  Add default directory to the given name
      if (abbr_name(1:1).eq.'/') then
         temp_name = abbr_name
      else if (def_dir.ne.' ') then
         ls = chr_lenb(def_dir)
         if (def_dir(ls:ls).eq.'/') then
            temp_name = def_dir(1:ls) // abbr_name
         else
            temp_name = def_dir(1:ls) // '/' // abbr_name
         end if
      else
         temp_name = '/mraos*/data/*/' // abbr_name
      end if

C  If the name ends in '/' or '*', then search directory
      ls = chr_lenb(temp_name)
      if (temp_name(ls:ls).eq.'*') then
         ls = chr_ilstc(temp_name,'/')
      end if
      if (temp_name(ls:ls).eq.'/') then
         temp_name(ls+1:) = '*'
      end if

      call io_namfil( temp_name, file_name, 0, s )


*      if (s.eq.NO_FILE .or. s.eq.AMB_FILE) then
C    Check for matching abbreviations in this directory
*         s = 0
*         i = 0
*         count = 0
*         ls = chr_ilstc(temp_name,'/')
*         temp_name(ls+1:) = '*'
*  100    continue
*            call io_nxtfil( temp_name, file_name, objx, count, s )
*            if (s.eq.0) then
*               if (chr_fmatch( abbr_name, file_name)) then
*                  i = i + 1
*                  ls = chr_lenb(file_name)
*                  sf_list(i) = file_name(1:ls)
*               end if
*               if (i.lt.max_sf) goto 100
*            end if
*         num_match = i
*         if (num_match .eq. 0) then
*            s = NO_FILE
*         else if (num_match .eq. 1) then
*            s = 0
*            file_name = sf_list(1)
*         else if (num_match .gt. 1) then
*            s = AMB_FILE
*         end if
*      end if

*      if (s.eq.AMB_FILE .and. amb_flag .and. .not.batch) then
C    Resolve ambiguous file reference
*         s = 0
*         i = 0
*  200    continue
*            i = i + 1
*            ls = chr_lenb(sf_list(i))
*            if (io_yesno(sf_list(i)(1:ls)//'?','no',s )) then
*               file_name =  sf_list(i)
*            else if (i.lt.num_match) then
*               goto 200
*            else
*               s = AMB_FILE
*            end if
*      end if


C  Add sample file extension to full name, update default directory
      if (s .eq. 0) then
         temp_name = file_name
         ls = chr_ilstc(file_name,'/')
         if (file_name(ls:) .ne. '/s5'   .and.
     :       file_name(ls:) .ne. '/s15'  .and.
     :       file_name(ls:) .ne. '/s27'  .and.
     :       file_name(ls:) .ne. '/s31'  .and.
     :       file_name(ls:) .ne. '/samp' .and.
     :       file_name(ls:) .ne. '/cal'  .and.
     :       file_name(ls:) .ne. '/rem')      then
            def_dir = file_name(1:ls-1)
            ls = chr_lenb(file_name)
            temp_name = file_name(1:ls) // '/s*'
         end if
         call io_namfil( temp_name, full_name, 0, s )
      end if

      if (s .ne. 0) full_name = ' '

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 999  call smp_wrerr( s, 'in subroutine ENQ_SFNAME' )

      end
