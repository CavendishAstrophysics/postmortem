
*  pointing analysis: data block
*  -----------------------------

*  GP 30 April 93

      character*64 pointing_data
      parameter   (pointing_data = '(Telescope)pointing:data')

      integer     max_point, bytesperblock
      parameter  (max_point = 127, bytesperblock = (max_point+1)*4)

      integer*4   pointing(0:max_point)

      integer*4   index   ! = +block no for valid data
*                         ! = -block no for deleted data

      character*16 source_name
      character*64 comment

* UT at start of obs from Itim1 (sec,min,hour), Idat1 (day,month,year)
      integer*4   time(3), date(3)
      integer*4   o_angle
      real*4      ra4date, dec4date, ha1, ha2, obs_freq
      real*4      mean_wind, min_wind, max_wind
      integer*2   o_aerial(8)     ! o_aerial(i) = 1 if data for ae i

      real*4      mHA(8), cHA(8), fitHA(8), mDec(8), cDec(8), fitDec(8)

      equivalence (pointing(0),  index)
      equivalence (pointing(1),  time(1))
      equivalence (pointing(4),  date(1))
      equivalence (pointing(7),  ra4date)
      equivalence (pointing(8),  dec4date)
      equivalence (pointing(9),  ha1)
      equivalence (pointing(10), ha2)
      equivalence (pointing(11), o_angle)
      equivalence (pointing(12), o_aerial)
      equivalence (pointing(16), obs_freq)
      equivalence (pointing(17), mean_wind)
      equivalence (pointing(18), min_wind)
      equivalence (pointing(19), max_wind)
* free                                        ! 20 - 43
      equivalence (pointing(44), source_name) ! 44 - 47
      equivalence (pointing(48), comment)     ! 48 - 63

      equivalence (pointing(64), mHA(1))
      equivalence (pointing(72), cHA(1))
      equivalence (pointing(80), fitHA(1))
      equivalence (pointing(88), mDec(1))
      equivalence (pointing(96), cDec(1))
      equivalence (pointing(104),fitDec(1))
* free                                        ! 112 - 127

