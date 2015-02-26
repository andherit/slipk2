module makepdf
!######################################################
! Author : André Herrero
! Contact : andherit@gmail.com, andre.herrero@ingv.it
! Public Domain (CC0 1.0 Universal)
!######################################################

  type slippdfinputs
    integer :: gn
    real*4, dimension(:,:), allocatable :: gauss   
  end type slippdfinputs

contains

!***********************************************************
subroutine faultpdf(spdfi,fp,m,n,dx,sws)
  implicit none
      
!  read a list of sgn 2D gaussian defined by their
!  center (xi,yi), their sigma (sigi) iand their weight (wgti)
!  to construct on the fault plane, a pdf compatible with
!  the subroutine pdfslipfract.
!  The fault plane is defined through an array fp(m,n). The fault
!  dimension is (m-1)dx*(n-1)dx in the node mode (sws=1) or
!  mdx*ndx in the pixel mode

   integer :: m,n,sws
   type(slippdfinputs) :: spdfi
   real*4, dimension(m*n) :: fp
   real*4 :: dx

   real*4, parameter :: pi=acos(-1.)
   integer :: i,j,k,idx
   real*4 :: xl,yl,dst2,unormgauss,fpint

   fp=0.
   fpint=0.
   do i=1,m
      if (sws == 1) then
         xl=(i-1)*dx
      else
         xl=(i-.5)*dx
      endif
      do j=1,n
         if (sws == 1) then
            yl=(j-1)*dx
         else
            yl=(j-.5)*dx
         endif
         idx=j+(i-1)*n
         do k=1,spdfi%gn
            dst2=(yl-spdfi%gauss(k,2))**2.+(xl-spdfi%gauss(k,1))**2.
            unormgauss=exp(-dst2/2./spdfi%gauss(k,3)**2.)
            fp(idx)=fp(idx)+unormgauss*spdfi%gauss(k,4)
         enddo
         fpint=fpint+fp(idx)
      enddo
   enddo
   do i=1,m*n
      fp(i)=fp(i)/fpint
   enddo  
   return
end subroutine faultpdf
end module makepdf
