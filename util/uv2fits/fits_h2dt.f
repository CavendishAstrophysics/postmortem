*+FITS_H2DT - uv header to scratch file on disc, and to tape
*
*
       subroutine fits_h2dt(status)
*
*
*
*  S.Kenderdine    2 August 1988   Version 0.1
*  P.Alexander     12 January 1990 Version 1.0
*                  All access is via enquiry routines
*
*-

       integer status

       include '/mrao/post/include/global_constants.inc'
       include '/mrao/post/include/src_pack.inc'
       include '/mrao/include/constants.inc'
       include 'uv2fits_common.inc'

       character object*8
       character scratch*30
       character telescope*8
       character units*8
       character centre(max_centre)*24
       character sfile(max_centre)*40

       integer i,iaxis
       integer telcode
       integer icentre,ncentre,nsamp

       real poln

       real*8 alpha
       real*8 delta,dec,decb,decd
       real*8 freq1,freq2,dfreq
       real*8 epoch
       real*8 mjd0
       real*8 ra,rab,rad,obsdat,refdat
       real*8 ra_ref(max_centre)
       real*8 dec_ref(max_centre)
       real*8 zero,one

       character stokes(7)*4
       data      stokes / 'BEAM', 'I', 'Q', 'U', 'V', 'I-Q', 'I+Q' /

       if (status.ne.0) return

* Entry

c      scratch='scr:out'
c      call io_opefil(ddev,scratch,'write',0,status)
       call io_opescr(ddev,scratch,'write',0,status)

* Initial items

       bitpix=32
       naxis=6
       naxisn(1)=0
       naxisn(2)=3
       naxisn(3)=1
       naxisn(4)=nch
       naxisn(5)=1
       naxisn(6)=1
       if (nsb.gt.1) then
         naxisn(5)=nsb
         naxisn(6)=1
         naxisn(7)=1
         naxis=7
       endif

       write(ddev,1000) bitpix,naxis,(naxisn(i),i=1,4)
 1000  format(
     :  'SIMPLE  = ',19x,'T',1x,'/ Standard FITS format'/
     :  'BITPIX  = ',18x,i2,1x,'/ Bits per pixel'/
     :  'NAXIS   = ',17x,i3,1x,'/ Number of axes'/
     :  'NAXIS1  = ',16x,i4,1x,'/ No image; uv data'/
     :  'NAXIS2  = ',16x,i4,1x,'/ # complex: cos, sin, weight'/
     :  'NAXIS3  = ',16x,i4,1x,'/ Number of polarisations'/
     :  'NAXIS4  = ',16x,i4,1x,'/ Number of frequencies')
        iaxis = 5

        if (nsb.gt.1) then
          write(ddev,1001) naxisn(iaxis)
 1001     format('NAXIS5  = ',16x,i4,1x,'/ Number of subbands')
          iaxis = iaxis+1
        endif

        write(ddev,1002) (i,naxisn(i),i=iaxis,naxis)
 1002   format(
     :  'NAXIS',i1,'  = ',16x,i4,1x,'/ RA(1950)'/
     :  'NAXIS',i1,'  = ',16x,i4,1x,'/ Dec(1950)'/
     :  'BLOCKED = ',19x,'T',1x,'/ Tape may be blocked')

* Obtain source packing information

       call enq_src_pack(lun,src_num,src_pack,status)

* Groups format

       write(ddev,1003) pcount,gcount
 1003  format(
     :  'GROUPS  = ',19x,'T',1x,'/ Groups data structure'/
     :  'PCOUNT  = ',16x,i4,1x,'/ Parameters per group'/
     :  'GCOUNT  = ',14x,i6,1x,'/ Number of groups'/
     :  'EXTEND  = ',19x,'T',1x,'/ Extension is antenna table')

* Object, telescope, date of observation

       call lsf_enq_pc_epoch(lsf_num,obsdat,rad,decd,object,status)
       call lsf_enq_pc_rdate(lsf_num,refdat,rab,decb,object,status)
       call enq_tscope(lun,telescope,telcode,status)
       if (telescope.eq.'T151' .or. telescope.eq.'38MHZ') then
         telescope = 'CLFST'
       endif

       write(ddev,1004) object,telescope,telescope,
     :  idate1(1),idate1(2),mod(idate1(3),1900)
 1004  format(
     :  'OBJECT  = ',1h',a,1h',11x,'/ Source name'/
     :  'TELESCOP= ',1h',a,1h',11x,'/ Radio telescope'/
     :  'INSTRUME= ',1h',a,1h',11x,'/ Instrument'/
     :  'DATE-OBS= ',1h',2(i2.2,'/'),i2.2,1h',11x,
     :                             '/ Mean date of observations')

* Bunit, bzero, bscale, blank, epoch

       call enq_units(lun,units,status)
       call chr_chucas(units)
       bzero=0d0
*      bscale=1d0/src_amp_factor
* Scaling adjusted to make use of 32-bit dynamic range
*      bscale=1d0/(2.d4*src_amp_factor)
       bscale=(aclip)/2.d9
c     write(*,*)'aclip',aclip
c     write(*,*)'bscale',bscale
       write(ddev,1005) units,bzero,bscale
 1005  format(
     :  'BUNIT   = ',1h',a,1h',11x,'/ Units of data'/
     :  'BZERO   = ',4x,1pe16.9,1x,'/ Data offset'/
     :  'BSCALE  = ',4x,1pe16.9,1x,'/ Data = tape*BSCALE + BZERO')

       blank=-2147483648
       if(blank.ne.0) write(ddev,1006) blank
 1006  format(
     :  'BLANK   = ',8x,i12,1x,'/ Undefined values on tape')

       epoch=1950.0d0
       write(ddev,1007) epoch
 1007  format(
     :  'EPOCH   = ',10x,f10.4,1x,'/ Epoch of RA, Dec')

* Pointing centre

       call enq_centres(lun,ncentre,centre,sfile,nsamp,
     :                      ra_ref,dec_ref,icentre,status)
       write(ddev,1008) ra_ref(icentre)/const_d2r,
     :                  dec_ref(icentre)/const_d2r
 1008  format(
     :  'OBSRA   = ',4x,1pe16.9,1x,'/ Antenna pointing RA'/
     :  'OBSDEC  = ',4x,1pe16.9,1x,'/ Antenna pointing DEC')

* Precess map centre from reference data to epoch

       ra=rab
       dec=decb
       if(dabs(refdat-1950.0d0).gt.1.0d-6) then
         call precrd(refdat,ra,dec,1950.d0,alpha,delta)
         ra=alpha
         dec=delta
       endif
       if(dabs(epoch-1950.0d0).gt.1.0d-6) then
         call precrd(1950.d0,ra,dec,epoch,alpha,delta)
         ra=alpha
         dec=delta
       endif

       one=1d0
       zero=0d0
       dfreq=one
       call enq_chfreq(lun,isb(1),ich(1),freq1,status)
       call enq_chfreq(lun,isb(1),ich(nch),freq2,status)
       if (nch.gt.1) dfreq=(freq2-freq1)/(nch-1)
c     write(*,'(3E14.6,I6)')freq1,freq2,dfreq

* Ctype etc

       write(ddev,1009) one,one,one,zero
 1009  format(
     :  'CTYPE2  = ',10h'COMPLEX ',11x,'/'/
     :  'CRVAL2  = ',4x,1pe16.9,1x,'/'/
     :  'CDELT2  = ',4x,1pe16.9,1x,'/'/
     :  'CRPIX2  = ',4x,1pe16.9,1x,'/'/
     :  'CROTA2  = ',4x,1pe16.9,1x,'/')

       poln=1
       write(ddev,1010) stokes(poln),poln,one,one,zero
 1010  format(
     :  'CTYPE3  = ',10h'STOKES  ',11x,'/ Stokes parameter : ',A/
     :  'CRVAL3  = ',4x,1pe16.9,1x,'/'/
     :  'CDELT3  = ',4x,1pe16.9,1x,'/'/
     :  'CRPIX3  = ',4x,1pe16.9,1x,'/'/
     :  'CROTA3  = ',4x,1pe16.9,1x,'/')

       write(ddev,1011) freq1,dfreq,one,zero
 1011  format(
     :  'CTYPE4  = ',10h'FREQ    ',11x,'/ Frequency, Hertz'/
     :  'CRVAL4  = ',4x,1pe16.9,1x,'/'/
     :  'CDELT4  = ',4x,1pe16.9,1x,'/'/
     :  'CRPIX4  = ',4x,1pe16.9,1x,'/'/
     :  'CROTA4  = ',4x,1pe16.9,1x,'/')

       iaxis = 5
       if (nsb.gt.1) then
         write(ddev,1012) one,one,one,zero
 1012    format(
     :    'CTYPE5  = ',10h'IF      ',11x,'/'/
     :    'CRVAL5  = ',4x,1pe16.9,1x,'/'/
     :    'CDELT5  = ',4x,1pe16.9,1x,'/'/
     :    'CRPIX5  = ',4x,1pe16.9,1x,'/'/
     :    'CROTA5  = ',4x,1pe16.9,1x,'/')
         iaxis = iaxis+1
       endif

       write(ddev,1013) iaxis,iaxis,ra/const_d2r,
     :                  iaxis,one,iaxis,one,iaxis,zero
 1013  format(
     :  'CTYPE',i1,'  = ',10h'RA      ',11x,
     :                                    '/ Right Ascension, degrees'/
     :  'CRVAL',i1,'  = ',4x,1pe16.9,1x,'/'/
     :  'CDELT',i1,'  = ',4x,1pe16.9,1x,'/'/
     :  'CRPIX',i1,'  = ',4x,1pe16.9,1x,'/'/
     :  'CROTA',i1,'  = ',4x,1pe16.9,1x,'/')
       iaxis = iaxis+1

       write(ddev,1014) iaxis,iaxis,dec/const_d2r,
     :                  iaxis,one,iaxis,one,iaxis,zero
 1014  format(
     :  'CTYPE',i1,'  = ',10h'DEC     ',11x,'/ Declination, degrees'/
     :  'CRVAL',i1,'  = ',4x,1pe16.9,1x,'/'/
     :  'CDELT',i1,'  = ',4x,1pe16.9,1x,'/'/
     :  'CRPIX',i1,'  = ',4x,1pe16.9,1x,'/'/
     :  'CROTA',i1,'  = ',4x,1pe16.9,1x,'/')

* Parameter details

c uscal now estimated in main program
c      uscal=1.0d+0/(4.0d11)
       uzero=0.0d+0
       write(ddev,1051) uscal,uzero
 1051  format(
     :  'PTYPE1  = ',10h'UU      ',11x,'/ U coordinate, seconds'/
     :  'PSCAL1  = ',4x,1pe16.9,1x,'/ Real = TAPE*PSCAL + PZERO'/
     :  'PZERO1  = ',4x,1pe16.9,1x,'/')

       vscal=uscal
       vzero=0.0
       write(ddev,1052) vscal,vzero
 1052  format(
     :  'PTYPE2  = ',10h'VV      ',11x,'/ V coordinate, seconds'/
     :  'PSCAL2  = ',4x,1pe16.9,1x,'/ Real = TAPE*PSCAL + PZERO'/
     :  'PZERO2  = ',4x,1pe16.9,1x,'/')

       wscal=uscal
       wzero=0.0
       write(ddev,1053) wscal,wzero
 1053  format(
     :  'PTYPE3  = ',10h'WW      ',11x,'/ W coordinate, seconds'/
     :  'PSCAL3  = ',4x,1pe16.9,1x,'/ Real = TAPE*PSCAL + PZERO'/
     :  'PZERO3  = ',4x,1pe16.9,1x,'/')

* Date parameter, as two entries:
*  ptype1: pzero1 = intpt(jd at start-time of observations)
*          pscal1 = 1/128 days
*          value of type1 parameter = jd of sample to nearest
*             integral value of 1/128 days
*  ptype2: pzero2 = 0
*          pscal2 = 1/6e6; precision is about 0.01 second

       call enq_mjd_st0(lun,mjd0,status)
       mjd2jd=2.4d6+0.5d0
       jd0=mjd0+mjd2jd
       ijd0=jd0
       jdazero=ijd0
       jdascal=1d0/128d0
       jdbscal=1d0/6d6
       write(ddev,1054) jdascal,jdazero,jdbscal,zero
 1054  format(
     :  'PTYPE4  = ',10h'DATE    ',11x,'/ Julian Date' /
     :  'PSCAL4  = ',4x,1pe16.9,1x,'/'/
     :  'PZERO4  = ',4x,1pe16.9,1x,'/'/
     :  'PTYPE5  = ',10h'DATE    ',11x,'/ Julian Date'/
     :  'PSCAL5  = ',4x,1pe16.9,1x,'/'/
     :  'PZERO5  = ',4x,1pe16.9,1x,'/')

       write(ddev,1055) one,zero
 1055  format(
     :  'PTYPE6  = ',10h'BASELINE',11x,'/ Ant1*256 + Ant2'/
     :  'PSCAL6  = ',4x,1pe16.9,1x,'/'/
     :  'PZERO6  = ',4x,1pe16.9,1x,'/')

* History, for AIPS sort-order etc

       write(ddev,1056) 1.d0
 1056  format(
     :  'HISTORY AIPS  SORT ORDER = ''TB'''/
     :  '                   / Where T is time (IAT), B is baseline num'/
     :  'HISTORY AIPS WTSCAL = ',4x,1pd16.9/
     :  '                   / Complex wts = WTSCAL*(TAPE*BSCALE+BZERO)')

* End of generating header to scratch file

       write(ddev,1090)
 1090  format('END')

       endfile(ddev)
       rewind(ddev)

* Read from disc to tape

       call fits_d2t('begin',i2line,0,status)

 200   read(ddev,1100,end=290) line
 1100  format(a)
       call fits_d2t('write',i2line,40,status)
       goto 200

* End of header

 290   line=' '
       call fits_d2t('fill',i2line,0,status)

       close(ddev)

       end
