! last modified 18 Aug 2005   GP

^procedure display-all     /mrao/post/util/display_all.com

^procedure cal-m           /mrao/post/util/cal-m.com
^procedure cal-sz          /mrao/post/util/cal-sz.com
^procedure cal-offset      /mrao/post/util/cal-offset-5pt.com
^procedure cal-raster      /mrao/post/util/cal-raster.com
^procedure cal-3-raster    /mrao/post/util/cal-3-raster.com
^procedure cal-mosaic      /mrao/post/util/cal-mosaic.com
^procedure set-flux        /mrao/post/util/set-flux.com
^procedure ng              /mrao/post/util/new-geometry.com
^procedure retro           /mrao/post/util/retro.com
^procedure ar1             /mrao/post/util/ar1.com
^procedure ar2             /mrao/post/util/ar2.com
^procedure mb              /mrao/post/util/mb.com

^procedure cal-auto        /mrao/post/util/cal-auto.com
^procedure cal-single      /mrao/post/util/cal-single.com


^procedure display-a4      /home/guy/com/display-a4.com

^alias sf         set-sample-file
^alias cos       'set-display cos-sin'
^alias amplitude 'set-display amp-phase'
^alias psplot    'set-plot-device ~/temp.ps/ps'
^alias xterm     'set-plot-device /xterm'
^alias pericom   'set-display plot-device terminal/pericom'
^alias hplaser   '^system lp -c -d hplaser ~/temp.ps'
^alias rtlaser   '^system lp -c -d rtlaser ~/temp.ps'

^alias wind      'monitor check-wind,no,yes,,,'
^alias rain      'monitor check-rain,no,no,yes,all,,,'
^alias cryo      'monitor check-cryo,no,yes,all,,,'

