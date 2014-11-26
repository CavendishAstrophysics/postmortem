
C     *****************************************************************
C
C+lsf_set_spacings
C
      SUBROUTINE lsf_set_spacings(    lsf_num,
     *                                num_spac,
     *                                spacings,
     *                                set_flg,
     *                                s                )

C
C     Sets the spacing list for a logical sample file.
C
C     Given:
C         Logical sample file number
              integer             lsf_num
C         Number of spacings.
              integer             num_spac
C         Integer spacing list.
              integer             spacings(num_spac)

C     Returned:
C         Spacing list update type flag (see below)
              integer             set_flg
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     Sets the spacing list in the logical sample file. The spacings
C     are both given and returned in the array 'spacings'. Normally,
C     the difference between the given and returned list is just
C     the vetting out of spacings that are not in the lsf. The exact
C     action depends on the setting of the set_flg:
C
C     set_flg = 1   - the given list replaces the existing lsf spacing
C                     list. The value of 'spacings' and 'num_spac' are
C                     unchanged, except if they contain illegal values.
C     set_flg = 2   - the list replaces only the runtime version of
C                     the lsf spacing list. The permanent spacing list
C                     is unchanged, and only spacings that are in the
C                     permanent spacing list are allowed in the runtime
C                     list. The returned values of 'spacings' and
C                     'num_spac' are for the runtime list, after vetting
C                     by the permanent list.
C     set_flg = 3   - The runtime spacing list is set equal to the
C                     permanent spacing list and returned in 'spacings'.
C                     The input values of 'spacings' and 'num_spac' are
C                     irrelevant. In this mode this routine can be used
C                     as an enquiry routine.
C     set_flg = 4   - The spacings in the spacing list are excluded from
C                     the existing LSF spacing list. The value of
C                     'spacings' and 'num_spac' are unchanged, except if
C                     they contain illegal values.
C
C     NPR     8 October 1987.
C
C     pjw has added setting up of arrays in lsf-runtime for interference
C     checking. These are only relevant for type 5 - multi-level.
C                                                                20/9/90
C-
C     ****************************************************************
C
C     Function declarations

      logical         util_tstbit

C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/post/include/lsflib_errors.inc'
      include  '/mrao/post/include/lsf_definition.inc'
      include  '/mrao/post/include/lsf_runtime.inc'

C     ****************************************************************
C
C     Local variables, equivalences and commons
C         Loop counter
              integer         i, j
C         Number of valid spacings
              integer         valid_spac
C         Number of spacings in physical sample file.
              integer         num_sfsp
C         current spacing no.
              integer         spacing_no
              real            d_amp, d_sp
              real            pre_multi_level(2,5),post_multi_level(2,5)
              equivalence    (pre_int_chop_params, pre_multi_level)
              equivalence    (post_int_chop_params, post_multi_level)
C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

      if ( lsf_num .ne. curr_lsf_num ) then
          call get_lsf( lsf_num, s )
          if ( s .ne. 0 ) goto 9999
      end if

      call enq_numvis( sf_lun, num_sfsp, s )
      if (num_sfsp .gt. max_sp) then
          s = ILL_BUFFSIZE
          goto 9999
      end if

C     ****************************************************************
C
C         Main Code
C         ---------
C
      if ( set_flg .eq. 1 ) then
          lsf_key  = 1
          lsf_name = ' '

          call util_clrbfd( sp_bit_arr, 1, num_sfsp )

          valid_spac = 0
          do 200, i = 1, num_spac
              if (spacings(i).ge.1 .and. spacings(i).le.num_sfsp) then
                  call util_setbit( sp_bit_arr, spacings(i) )
                  valid_spac = valid_spac + 1
                  spacings(valid_spac) = spacings(i)
              end if
  200     continue
          num_spac = valid_spac
      else if (set_flg .eq. 2) then
          valid_spac = 0
          do 300, i = 1, num_spac
              if (spacings(i).ge.1 .and. spacings(i).le.num_sfsp) then
                  if ( util_tstbit( sp_bit_arr, spacings(i) ) ) then
                      valid_spac = valid_spac + 1
                      spacings( valid_spac ) = spacings( i )
                  end if
              end if
  300     continue
          num_spac = valid_spac
      else if (set_flg .eq. 3) then
          valid_spac = 0
          do 400, i = 1, num_sfsp
              if ( util_tstbit( sp_bit_arr, i ) ) then
                  valid_spac = valid_spac + 1
                  spacings( valid_spac ) = i
              end if
  400     continue
          num_spac = valid_spac
      else if ( set_flg .eq. 4 ) then
          lsf_key  = 1
          lsf_name = ' '

          do 500, i = 1, num_spac
              if (spacings(i).ge.1 .and. spacings(i).le.num_sfsp) then
                  if (util_tstbit(sp_bit_arr, spacings(i))) then
                      call util_clrbit( sp_bit_arr, spacings(i) )
                  end if
              end if
  500     continue

          valid_spac = 0
          do 600, i = 1, num_sfsp
              if ( util_tstbit( sp_bit_arr, i ) ) then
                  valid_spac = valid_spac + 1
                  spacings( valid_spac ) = i
              end if
  600     continue
          num_spac = valid_spac
      else
          s = ILL_FLAG
          goto 9999
      end if

      do 700, i = 1, num_spac
          sp_list(i) = spacings(i)
  700 continue
      sp_list_len = num_spac
      call enq_baselines( sf_lun,
     *                    sp_list_len, sp_list,
     *                    base, baseln_skew,
     *                    s                       )

C     if multi-level interference chop set up index values

      if ( pre_int_chop_type .eq. 5 ) then
C     count pairs
          i = 1
          do while ( ( i .le. 5 ).and.( pre_multi_level(1,i) .ne. -1 ))
              npair(1) = i
              amp_level(1,i) = pre_multi_level(1,i)
              i = i + 1
          end do

        if ( num_spac .eq. 1 ) then
C     Deal with single spacing case by putting the spacing's
C     interpolated clip into amp_level
          call enq_isp_code( sf_lun, sp_list(1), spacing_no, s )
          i = 1
          do while ( ( i .le. npair(1) )
     *             .and. ( pre_multi_level(2,i) .lt. spacing_no ) )
              i = i + 1
          enddo
          if ( i .eq. 1 ) then
              amp_level(1,1) = pre_multi_level(1,1)
              sp_index(1,1) = 2
          elseif ( i .gt. npair(1) ) then
              amp_level(1,1) = pre_multi_level(1,npair(1))
              sp_index(1,1) = 2
          else
              d_amp = pre_multi_level(1,i) - pre_multi_level(1,i-1)
              d_sp  = pre_multi_level(2,i) - pre_multi_level(2,i-1)
              amp_level(1,1) = pre_multi_level(1,i-1)
     *                        + ( spacing_no - pre_multi_level(2,i-1) )
     *                           * d_amp / d_sp
              sp_index(1,1) = 2
          endif
        else
          j = 1
          do i = 1, num_spac
              call enq_isp_code( sf_lun, i, spacing_no, s )
              if ( spacing_no .eq. pre_multi_level(2,j) ) then
                  sp_index(1,j) = i
                  if( j .lt. npair(1) ) then
                        j = j + 1
                  else
                      goto 801
                  endif
              endif
          end do
        endif
801     continue
      endif

      if ( post_int_chop_type .eq. 5 ) then

          i = 1
          do while ( ( i .le. 5 ).and.( post_multi_level(1,i) .ne. -1 ))
              npair(2) = i
              amp_level(2,i) = post_multi_level(1,i)
              i = i + 1
          end do
        if ( num_spac .eq. 1 ) then
C     Deal with single spacing case by putting the spacing's
C     interpolated clip into amp_level
          call enq_isp_code( sf_lun, sp_list(1), spacing_no, s )
          i = 1
          do while ( ( i .le. npair(2) )
     *             .and. ( post_multi_level(2,i) .lt. spacing_no ) )
              i = i + 1
          enddo
          if ( i .eq. 1 ) then
              amp_level(2,1) = post_multi_level(1,1)
              sp_index(2,1) = 2
          elseif ( i .gt. npair(2) ) then
              amp_level(2,1) = post_multi_level(1,npair(2))
              sp_index(2,1) = 2
          else
              d_amp = post_multi_level(1,i) - post_multi_level(1,i-1)
              d_sp  = post_multi_level(2,i) - post_multi_level(2,i-1)
              amp_level(2,1) = post_multi_level(1,i-1)
     *                        + ( spacing_no - post_multi_level(2,i-1))
     *                            * d_amp / d_sp
              sp_index(2,1) = 2
          endif
        else
          j = 1
          do i = 1, num_spac
              call enq_isp_code( sf_lun, i, spacing_no, s )
              if ( spacing_no .eq. post_multi_level(2,j) ) then
                  sp_index(2,j) = i
                  if( j .lt. npair(2) ) then
                        j = j + 1
                  else
                      goto 802
                  endif
              endif
          end do
        endif
802     continue
      endif

      if ( s .ne. 0 ) goto 9999

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call lsf_wrerr( s, 'in subroutine LSF_SET_SPACINGS' )
      end
