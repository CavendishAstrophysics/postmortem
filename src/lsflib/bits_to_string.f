
C     *****************************************************************
C
C+bits_to_string
C
      SUBROUTINE bits_to_string ( bits,
     *                            max_list,
     *                            spac_flg,
     *                            string,
     *                            s                      )

C
C     Translates a bit array into a string description.
C
C     Given:
C         Maximum element in bit array.
              integer             max_list
C         Bit array to be translated.
              integer*4           bits( (max_list-1)/32+1 )
C         Flag set if list is to be interpreted as spacings.
              logical             spac_flg

C     Returned:
C         String list
              character*(*)       string
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     Converts a list expressed as a bit array into a string.
C
C     PA, 6/3/89
C
C-
C     ****************************************************************
C
C     Function declarations
C
      include  '/mrao/include/chrlib_functions.inc'
      logical         util_tstbit

C     ****************************************************************
C
C     Global common blocks.

      include    '/mrao/post/include/lsf_runtime.inc'

C     ****************************************************************
C
C     Local constant and variable declarations
C         String length,, list element to write.
              integer         ls, ln, i, len_str
              character*12    list_element
C         Information for identifying visibility
              integer         nae, nsp, nba, nch, isp, iba, ich
              integer         i0, nskip
              character*1     sub_bands(5)
              data            sub_bands / 'A','B','C','D','E' /

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return
      string  = ' '
      len_str = len(string)

C     ****************************************************************
C
C         Main Code
C         ---------
C
C     Advance to first element in list.
      i  = 1
      i0 = 1
      ls = 1
      do while ( i .le. max_list .and. .not. util_tstbit(bits,i))
          i  = i+1
          i0 = i
      end do

C     Initialise for spacing list
      if (spac_flg) then
         call enq_obsdef( sf_lun, nae, nsp, nba, nch, s)
         nskip = nba*nch
      else
         nskip = 1
      end if

C     Move through the list
      do while ( i.le.max_list .and. ls.lt.len_str )
          if (spac_flg) then
              call enq_vis_desig( sf_lun, i, isp, iba, ich, s )
              call chr_chitoc( isp, list_element, ln )
          else
              call chr_chitoc( i, list_element, ln )
          end if

          if ((ls+ln).le.len_str) then
              string(ls:ls)      = ','
              string(ls+1:ls+ln) = list_element(1:ln)
          end if
          ls = ls+ln+1
          i = i+nskip

          if (i .le. max_list .and. util_tstbit(bits,i)) then
              do while ( i.lt.max_list .and. util_tstbit(bits,i+nskip))
                  i = i+nskip
              end do

              if (spac_flg) then
                 call enq_vis_desig( sf_lun, i, isp, iba, ich, s )
                 call chr_chitoc( isp, list_element, ln )
              else
                  call chr_chitoc( i, list_element, ln )
              end if

              if ((ls+ln).le.len_str) then
                  string(ls:ls)      = '-'
                  string(ls+1:ls+ln) = list_element(1:ln)
              end if
              ls = ls+ln+1
              i = i+nskip
          end if

C         Advance to next element
          do while ( i .le. max_list .and. .not. util_tstbit(bits,i))
              i = i+nskip
          end do
      end do

      string(1:1) = ' '
      call chr_chcomp( string, ls )

C     If spacing list determine sub-bands and channels
      if (spac_flg) then
        if (ls+6 .le. len_str) then
          string(ls+1:ls+5) = ' SB: '
          ls = ls + 6
          do i = i0, i0+nch*(nba-1),nch
            if (util_tstbit(bits,i)) then
              call enq_vis_desig( sf_lun, i, isp, iba, ich, s )
              if (ls.le.len_str) then
                string(ls:ls) = sub_bands(iba)
                ls = ls + 1
              end if
            end if
          end do
        end if

        if (ls+6 .le. len_str) then
          string(ls+1:ls+5) = ' CH: '
          ls = ls + 6
          do i = i0, i0+nch-1
            if (util_tstbit(bits,i)) then
              call enq_vis_desig( sf_lun, i, isp, iba, ich, s )
              if (ls.le.len_str) then
                write(string(ls:ls),'(I1)') ich
                ls = ls + 1
              end if
            end if
          end do
        end if

      end if

      if (s .ne. 0) goto 9999
      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call lsf_wrerr( s, 'in subroutine BITS_TO_STRING' )
          return
      end
