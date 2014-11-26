*+degrid

       SUBROUTINE degrid( REDTAPE, DATA, UV, DEGRID_TYPE, c_spec, c_pa,
     *                        RESULT, S )
C      ---------------------------------------------------------------
C
C     Returns a map(real) or aperture(complex) value at the given
C     u, v point. Linear or non-linear interpolation can be selected
C
C     Given:
C         Abbreviated map redtape - see (library)maplib-minirt:incl
              INTEGER     REDTAPE(*)
C         Map data
              REAL        DATA(*)
C         U, V position to find value of.
              REAL*8      UV(2)
C         Function to use for degridding.
              INTEGER     DEGRID_TYPE
C         degridding fn. specification ie args for tabfn
              integer     c_spec(3)
              real        c_pa(2)
C
C     Returned:
C         Returned value - complex if an aperture, real if a map.
              REAL        RESULT(*)
C         Status - must be zero on entry.
              INTEGER     S
C
C     Degrids the map data to a given, non-integral uv point in a map
C     or aperture.  At present, there are two types of degridding
C     function available :
C
C     degrid_type 1 - linear degridding from the four nearest pixels.
C     degrid_type 2 - degridding using a tabulated gaussian-sinc
C                     degridding function. The first zero of the
C                     function is at a radius of 1 and the standard
C                     deviation of the gaussian is sqrt(0.5/0.275). The
C                     tabulation is oversampled by a factor of 100 and
C                     has a halfwidth of 3 pixels. U, V near the edge
C                     of the map are interpolated linearly and any
C                     undefined pixels are ignored.
C     degrid_type 3 - as 2 above except that the actual type and details
C                     of the degridding function are passed in.
C
C     NPR     10 August 1988.
C     PJW     9/3/92
*-
C     ******************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/include/constants.inc'
      include  '/mrao/include/maplib_minirt.inc'
      include  '/mrao/include/maplib_errors.inc'
      include  '/mrao/include/maplib_subgrid.inc'

C     ****************************************************************
C
C     Local variables, equivilances and commons
C         Logical equivalent of dtp_mrt input parameter.
              logical         comp_flg
C         Real and complex parts of final value
              real            value, imag_value
C         Integral and fractional part of u and v
              integer         iu, iv
              real            deltu, deltv
C         Calculation variables for linear interpolation
              real            a,b,c,d,e,f
C         Variables used for pointing into the map array,
              integer         data_ptr, elm_length, row_length,start_row
C         Variables for pointing into the convolution fn table
              integer         start_conv_u, start_conv_v
              integer         end_conv_u, end_conv_v
              integer         conv_v_ptr, conv_u_ptr
C         Convolution function value for the current row and point
              real            conv_v, conv_val
C         locally read values of degriding fn. type and parameters
C              integer         c_tp, c_os, c_hw
C              real            c_pa(2)

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

      do 10, iu = 1, minirt_len
          minirt(iu) = redtape(iu)
   10 continue

C     ****************************************************************
C
C         Main Code
C         ---------
C
      comp_flg = (dtp_mrt .eq. 4)

      iu = int(uv(1))
      if (uv(1).lt.0.0D0) iu = iu-1
      iu = max( u1_mrt, min( u2_mrt-1, iu ))
      deltu = uv(1) - real(iu)

      iv = int(uv(2))
      if (uv(2).lt.0.0D0) iv = iv-1
      iv = max( v2_mrt, min( v1_mrt-1, iv ))
      deltv = uv(2) - real(iv)

      if (comp_flg) then
          elm_length = 2
      else
          elm_length = 1
      end if
      row_length = nx_mrt*elm_length

      if ( deltu.lt.0.0.or.deltu.gt.1.0 .or.
     *     deltv.lt.0.0.or.deltv.gt.1.0       ) then
          s = UV_OUTMAP
          value = blk_mrt
          if (comp_flg) imag_value = blk_mrt
      else if ( (degrid_type .eq. 1) .or.
     *          (iu-2).lt.u1_mrt .or. (iu+3).gt.u2_mrt .or.
     *          (iv-2).lt.v2_mrt .or. (iv+3).gt.v1_mrt     ) then
          data_ptr = (v1_mrt-iv-1)*row_length+(iu-u1_mrt)*elm_length+1

          a=data(data_ptr)
          b=data(data_ptr+           elm_length)
          c=data(data_ptr+row_length)
          d=data(data_ptr+row_length+elm_length)
          e=a+(b-a)*deltu
          f=c+(d-c)*deltu
          value=f+(e-f)*deltv

          if (comp_flg) then
              data_ptr = data_ptr + 1
              a=data(data_ptr)
              b=data(data_ptr+           elm_length)
              c=data(data_ptr+row_length)
              d=data(data_ptr+row_length+elm_length)
              e=a+(b-a)*deltu
              f=c+(d-c)*deltu
              imag_value=f+(e-f)*deltv
          end if
      else if (degrid_type .eq. 2  .or. degrid_type .eq. 3 ) then
C         Setup the convolution function if not yet done so.
          if ( .not. setup_fn ) then
              if( degrid_type .eq. 2 ) then
                  call tabfn( gauss_sinc, conv_os, conv_hw, conv_pa,
     *                         max_pts, conv, s                     )
              elseif( degrid_type .eq. 3 ) then
C                  call enconv( c_tp, c_os, c_hw, c_pa, s)
                  write(1,*)'type,over-sampling,half-width,pars:',
     *                        c_spec,c_pa
                  call tabfn( c_spec(1), c_spec(2), c_spec(3), c_pa,
     *                        max_pts, conv, s )
              endif
              setup_fn = .true.
          end if

C         Set up some array pointer variables prior to the convolution.
          start_conv_u = nint((real(1-conv_hw)-deltu)*real(conv_os))
          start_conv_v = nint((real(1-conv_hw)-deltv)*real(conv_os))
          end_conv_u   = start_conv_u + conv_pts
          end_conv_v   = start_conv_v + conv_pts

          data_ptr = (v1_mrt-(1-conv_hw+iv))*row_length +
     *               ((1-conv_hw+iu)-u1_mrt)*elm_length + 1
          start_row = data_ptr

C         Do the convolution
          value      = 0.0
          imag_value = 0.0

          do 200, conv_v_ptr = start_conv_v, end_conv_v, conv_os
              conv_v    = conv( conv_v_ptr )
              do 100, conv_u_ptr = start_conv_u, end_conv_u, conv_os
                  conv_val = conv(conv_u_ptr)*conv_v
                  if (data(data_ptr) .ne. blk_mrt) then
                      value = value + data(data_ptr)*conv_val
                      data_ptr = data_ptr + 1
                      if (comp_flg) then
                          imag_value= imag_value+data(data_ptr)*conv_val
                          data_ptr  = data_ptr + 1
                      end if
                  else
                      data_ptr = data_ptr + 1
                      if (comp_flg) data_ptr = data_ptr + 1
                  end if
  100         continue
              start_row = start_row - row_length
              data_ptr  = start_row
  200     continue
      else
          s = ILL_MAPFN
          goto 9999
      end if

      result(1) = value
      if (comp_flg) result(2) = imag_value

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call maperr( s, 'in routine degrid' )
          return
      end
