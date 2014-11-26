! set flux for (point) calibrator, and put it into the model
! 29 Oct 98

^request 'Cal-source flux density :'  '<Scal>'  Scal
model-source




<Scal>






!


