*+
      subroutine calc_shadow(file,shadow_beg,shadow_end,
     &       ha_beg_int,ha_end_int,run_beg_int,run_end_int,
     &       s)

c Subroutine which calculates the hour-angle at which the RT aerials
c become shadowed.
c
c Keith Grainge 9/11/95
*-

      include '/mrao/include/constants.inc'
      integer lun
      integer iae
      integer s
      real*8 xyz_coord(3)
      integer stime1(3)
      integer stime2(3)
      real*8 dec,ra
      real*8 geom_pars(10)
      character file*(*)

c other variables

      real*8 time_inc
      real*8 x_m(5),y_m(5),z_m(5)
      real*8 ha
      real*8 ha_beg(5),ha_end(5)
      integer ha_beg_int(5,3),ha_end_int(5,3)
      integer run_beg_int(3),run_end_int(3)
      real*8 delx_beg(5),dely_beg(5),delz_beg(5)
      real*8 delx_end(5),dely_end(5),delz_end(5)
      real*8 u,v
      real*8 proj_dist2
      real*8 dish_dia2
      real*8 start_time,finish_time
      real*8 ra_hours,dummy
      logical shadow_beg(5),shadow_end(5)
      integer i,j,k,nloop

c open file

      call open_sf(lun,file,'read',0,s)

c get information about telescope geometry - geom_pars(2) is the
c azimuth of the datum line (in equatorial plane)

      call enq_v2_telgeom ( geom_pars, s )

      time_inc = 0.005
      dish_dia2 = 12.8**2
      do i = 1,5
         shadow_beg(i) = .false.
         shadow_end(i) = .false.
         ha_beg(i) = 0.
         ha_end(i) = 0.
         do j = 1,3
            ha_beg_int(i,j) = 0
            ha_end_int(i,j) = 0
         end do
      end do


c get aerial positions. These are in 'system 2' coordinates.
c Convert to x_m, y_m, and z_m as required by program.
c xyz_coord in seconds. Convert to metres by multiplying by c.

      do iae = 1,5
         call enq_geometry(lun,iae,xyz_coord,s)
         x_m(iae) = -xyz_coord(1) * sin(geom_pars(2)) * const_c
         y_m(iae) = -xyz_coord(1) * cos(geom_pars(2)) * const_c
         z_m(iae) = xyz_coord(3) * const_c
      end do

c get ra and dec (at date)

      call enq_path_comp(lun,ra,dec,s)
      ra_hours = ra * 12./const_pi

c get run start and finish times

      call enq_sid_time(lun,stime1,stime2,s)

c ensure that finish time is higher than start time

      if (stime2(3).lt.stime1(3)) then
         stime2(3) = stime2(3) + 24
      end if

c if ra_hours is less than start time and if the difference is greater than
c 12 hours then add 24 hours to ra_hours

      dummy = dble(stime1(3)) + dble(stime1(2))/60. +
     &             dble(stime1(1))/3600.
      if ((ra_hours.lt.dummy).and.((dummy-ra_hours).gt.12.)) then
         ra_hours = ra_hours + 24.
      end if

c convert times to decimal

      start_time = dble(stime1(3)) + dble(stime1(2))/60. +
     &             dble(stime1(1))/3600.
      finish_time = dble(stime2(3)) + dble(stime2(2))/60. +
     &             dble(stime2(1))/3600.

c convert times to hour angles

      start_time = start_time - ra_hours
      finish_time = finish_time - ra_hours

c convert all angles to radians

      start_time = start_time * const_pi /12.
      finish_time = finish_time * const_pi /12.

      nloop = int((finish_time - start_time)/time_inc)
      ha = start_time

c generate distances on ground between adjacent aerials

c for beginning of run (ie 2 to 1; 3 to 2; etc)

      do j = 2,5
         k = j-1
         delx_beg(j) = x_m(j)-x_m(k)
         dely_beg(j) = y_m(j)-y_m(k)
         delz_beg(j) = z_m(j)-z_m(k)
      end do

c for end of run (ie 1 to 2; 2 to 3; etc)

      do j = 1,4
         k = j+1
         delx_end(j) = x_m(j)-x_m(k)
         dely_end(j) = y_m(j)-y_m(k)
         delz_end(j) = z_m(j)-z_m(k)
      end do

c loop over all times during run

      do i = 1,nloop

c check for shadowing at beginning of run (nb ae 1 never shadowed)

         do j = 2,5
            u = delx_beg(j) * sin(ha)
     &        + dely_beg(j) * cos(ha)
            v = dely_beg(j) * sin(ha) *sin(dec)
     &        - delx_beg(j) * cos(ha) * sin (dec)
     &        + delz_beg(j) * cos(dec)
            proj_dist2 = u**2 + v**2
            if ((proj_dist2.lt.dish_dia2).and.(ha.lt.0.)) then
               shadow_beg(j) = .true.
               ha_beg(j) = ha
            end if
         end do

c check for shadowing at end of run (nb ae 5 never shadowed)

         do j = 1,4
            u = delx_end(j) * sin(ha)
     &        + dely_end(j) * cos(ha)
            v = dely_end(j) * sin(ha) *sin(dec)
     &        - delx_end(j) * cos(ha) * sin (dec)
     &        + delz_end(j) * cos(dec)
            proj_dist2 = u**2 + v**2

c only looks for shadowing if shadowing at the end of the run has not yet been
c found ie only finds first instance of shadowing at end of run

            if ((proj_dist2.lt.dish_dia2).and.
     &          (shadow_end(j).eq..false.).and.(ha.gt.0.)) then
               shadow_end(j) = .true.
               ha_end(j) = ha
            end if
         end do

c increment ha

         ha = ha + time_inc

      end do

c flag an extra 5 minutes for safety and ensure that flagging continues
c to 5 minutes before start and 5 minutes after run

      do i = 1,5
         if (shadow_beg(i).eq..true.) then
            ha_beg(i) = ha_beg(i) + 0.02
         end if
         if (shadow_end(i).eq..true.) then
            ha_end(i) = ha_end(i) - 0.02
         end if
      end do

      start_time = start_time - 0.02
      finish_time = finish_time +0.02

c converts all hour angles from hour angles relative to local median to
c hour angles relative to equatorial plane (which is used by flag system
c in POSTMORTEM) by subtracting azdatum = geom_pars(2)

      start_time = start_time - geom_pars(2)
      finish_time = finish_time - geom_pars(2)
      do i = 1,5
         ha_beg(i) = ha_beg(i) - geom_pars(2)
         ha_end(i) = ha_end(i) - geom_pars(2)
      end do

c converts ha of shadowing and run times from radians to secs, mins, hours
c since ha_beg and run_beg are going to be negative add 24 hours to them

      do i = 1,5
         ha_beg(i) = ha_beg(i) * 12. /const_pi
         ha_end(i) = ha_end(i) * 12. /const_pi
         ha_beg(i) = ha_beg(i) +24.
         ha_beg_int(i,3) = int(ha_beg(i))
         ha_end_int(i,3) = int(ha_end(i))
         ha_beg(i) = (ha_beg(i) - dble(ha_beg_int(i,3)))*60.
         ha_end(i) = (ha_end(i) - dble(ha_end_int(i,3)))*60.
         ha_beg_int(i,2) = int(ha_beg(i))
         ha_end_int(i,2) = int(ha_end(i))
         ha_beg(i) = (ha_beg(i) - dble(ha_beg_int(i,2)))*60.
         ha_end(i) = (ha_end(i) - dble(ha_end_int(i,2)))*60.
         ha_beg_int(i,1) = int(ha_beg(i))
         ha_end_int(i,1) = int(ha_end(i))
      end do
      start_time = start_time * 12. /const_pi
      finish_time = finish_time * 12. /const_pi
      start_time = start_time + 24.
      run_beg_int(3) = int(start_time)
      run_end_int(3) = int(finish_time)
      start_time = (start_time - dble(run_beg_int(3)))*60.
      finish_time = (finish_time - dble(run_end_int(3)))*60.
      run_beg_int(2) = int(start_time)
      run_end_int(2) = int(finish_time)
      start_time = (start_time - dble(run_beg_int(2)))*60.
      finish_time = (finish_time - dble(run_end_int(2)))*60.
      run_beg_int(1) = int(start_time)
      run_end_int(1) = int(finish_time)

c close file

      call close_sf(lun,s)

      end


