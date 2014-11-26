C
C+grid_vis_buff
C
      SUBROUTINE grid_vis_buff(   vis_buffer,
     *                            uv_buffer,
     *                            num_vis,
     *                            aper,
     *                            n1, n2,
     *                            conv_hw, conv_os,
     *                            conv, num_pts,
     *                            weight_buffer,
     *                            beam,
     *                            s                  )

C     Grids a buffer of complex visibilities onto an aperture.
C
C     Given:
C         The number of visibilities to be gridded
              integer         num_vis
C         The buffer of complex visibities.
              complex         vis_buffer( num_vis )
C         uv coordinates of each visibility.
              real            uv_buffer( 2, num_vis )
C         The dimensions of the aperture.
              integer         n1, n2
C         The two dimensional aperture dimensions are aper(n1,n2) but
C         it is declared in 1D for speed - (0,0) is at (n1, n2/2+1)
              complex         aper( * )
C         The half width to convolve in the u and v coordinates
              integer         conv_hw
C         The oversampling of the convolution function.
              integer         conv_os
C         The half-width of the convolution array
              integer         num_pts
C         The tabulated convolution function - declared symmetrically:
              real            conv( -num_pts:num_pts )
C         The weights to be applied to the visibilities
              real            weight_buffer( num_vis )
C         The aperture of the beam.
              real            beam( * )

C     Returned:
C         Status variable - returned unchanged since no checking is
C         done because of time constraints.
              integer         s
C
C     Subroutine to grid a buffer of visibilities at the given uv
C     positions into the aperture array 'aper' using the convolution
C     function tabulated in conv.
C
C                                     Nick Rees - October 1986
C
C-
C     ****************************************************************
C
C     Local variable declarations
C         Loop counter and temporary storage variable
              integer         i
C         The current visibility being gridded
              complex         vis
              real            vis_real, vis_imag, vis_arr(2)
              equivalence   ( vis_arr(1), vis )
              equivalence   ( vis_arr(1), vis_real )
              equivalence   ( vis_arr(2), vis_imag )
C         The visibilities u, v and weight
              real            u, v, weight
C         The number of columns in the aperture
              integer         aper_cols
C         The u and v coordinates of the first element in the aperture
              real            u_first, v_first
C         Number of columns to grid minus 1
              integer         conv_width_m_1
C         The uv coordinates expressed in row-column coordinates.
              real            u_col, v_row
C         Increments to be added to the u and v coordinates to give
C         the boundaries of the gridding box.
              real            first_row_inc, first_col_inc
C         Parameters defining the gridding box.
              integer         first_row, last_row, row_inc,
     *                        first_col, last_col, end_col
C         The indexes in the 1D aperture array of the first element
C         on the first row to be gridded and similarly the first
C         element on the current row and the last row resp.
              integer         first_row_offset, row_offset,
     *                        last_row_offset
C         The index in the 1D aperture array where the the visibility
C         is currently being gridded to.
              integer         full_offset
C         The indexes in the convolution function table for the value
C         to use for the first column, the current column and the
C         current row.
              integer         first_c_u, conv_u, first_c_v, conv_v
C         The half width of the convolution table sampling in u and v
C         coordinates.
              real            delta
C         The "weight" to be attatched to this visibility on this row
C         and at this gridpoint.
              real            row_wt, vis_wt
C         A logical indicating the first pass for a given visibility.
              logical         first_time

C     ****************************************************************
C
C     Subroutine initialisation
C
      if (s .ne. 0) return

      u_first   = 1-n1
      v_first   = n2/2
      aper_cols = n1

      delta = 1/(2.0*real(conv_os))
      first_row_inc =  0.5 + v_first - float(conv_hw) - delta
      first_col_inc =  0.5 - u_first - float(conv_hw) - delta

C     ****************************************************************
C
C         Main Code
C         ---------
C
      do i = num_vis, 1, -1
          if ((vis_buffer(i)    .ne. (0.0,0.0)) .and.
     *        (weight_buffer(i) .ne.  0.0     )         ) then
              vis   = vis_buffer( i )
              u     = uv_buffer( 1, i )
              v     = uv_buffer( 2, i )
              if (u .gt. 0.0) then
                  u   = -u
                  v   = -v
                  vis = conjg( vis )
              end if
              weight= weight_buffer( i )
              first_time = .false.

  100         continue
                  first_time = .not. first_time
                  if (first_time) then
                      v_row     = - (v - v_first)
                      first_row = nint( first_row_inc - v )
                      first_c_v = nint((real(first_row)-v_row)*conv_os)
                      last_row  = first_row+conv_hw-first_c_v/conv_os
                      row_inc   = aper_cols

                      u_col     = + (u - u_first)
                      first_col = nint( first_col_inc + u )
                      first_c_u = nint((real(first_col)-u_col)*conv_os)
                      last_col  = first_col+conv_hw-first_c_u/conv_os
                      end_col   = min( last_col, aper_cols-1 )
                  else
                      vis       = conjg( vis )

                      first_row = n2-first_row
                      last_row  = n2-last_row
                      row_inc   =-aper_cols
                      first_c_u = (first_col-last_col)*conv_os-first_c_u
                      first_col = aper_cols+aper_cols-last_col-2
                  end if

                  conv_width_m_1  = end_col - first_col
                  first_row_offset= first_row*aper_cols + first_col + 1
                  last_row_offset = last_row *aper_cols + first_col + 1

                  conv_v = first_c_v
                  do row_offset = first_row_offset, last_row_offset,
     *                                                         row_inc
                      conv_u = first_c_u
                      row_wt = conv( conv_v ) * weight

                      do full_offset = row_offset,
     *                                     row_offset + conv_width_m_1
                          vis_wt   = row_wt * conv( conv_u )
                          beam(full_offset) = beam(full_offset) + vis_wt
                          aper(full_offset) = aper(full_offset)
     *                       + cmplx( vis_real*vis_wt, vis_imag*vis_wt )
                          conv_u   = conv_u + conv_os
                      end do
                      conv_v = conv_v + conv_os
                  end do
              if (first_time .and. end_col.eq.(aper_cols-1)) goto 100
          end if
      end do

C     Now ensure that central pixels of final column are hermitian
      row_inc = 0
      do i = (n2/2+1)*aper_cols,(n2/2+conv_hw+1)*aper_cols,aper_cols
          aper(i)         = (aper(i)+conjg(aper(i-row_inc)))/(2.0,0.0)
          beam(i)         = (beam(i)+beam(i-row_inc))/2.0
          aper(i-row_inc) = conjg(aper(i))
          beam(i-row_inc) = beam(i)
          row_inc         = row_inc+2*aper_cols
      end do

      if (s.eq.0) return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call map_wrerr( s, 'in subroutine GRID_VIS_BUFF' )
          return
      end
