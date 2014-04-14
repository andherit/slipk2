module pdf2slip
!######################################################
! Author : André Herrero
! Contact : andherit@gmail.com, andre.herrero@ingv.it
! Public Domain (CC0 1.0 Universal)
!######################################################
implicit none

contains

!***********************************************************
subroutine pdfslipfract(fp,m,n,dx,seed,moment,sd,mu,na,rmin,rmax,verb,sct)
  implicit none

! create a D2 fractal distribution of na asperities on
! the fault plane fp (dimension (m-1)(n-1)dxdx) taking
! into account a pdf distribution for the random
! process. The latter is initialized by a seed
! integer. The largest asperity is centred on the
! location of the maximum of the pdf. The normalisation
! is controlled by the moment or the stress drop (sd).
! Constants which may be tweak :
!       na : number of asperities
!       mu : the rigidity

  integer :: m,n,na,seed,verb,sct
  real*4 :: fp(m*n),dx,moment,sd,mu,rmin,rmax

  integer :: i,j,k,idxmax,lmin,lmax,wmin,wmax,lx,wx
  integer :: imax,jmax,idx
  real*4, dimension(:), allocatable :: r,pdf
  real*4, parameter :: pi=acos(-1.)
  real*4 :: p,d,slp,cslp
  real*4 :: pdfmax,curint,numrnd,mf,mg,mn

! scaling factor : moment or stress drop
  if (sct == 1) then
     moment=sd*dx**3.*(m-1)**2.*(n-1)
  else
     sd=moment/(dx**3.*(m-1)**2.*(n-1))
  endif
! initialization of the random generator
  call srand(seed)
! Eshelby's constant
  cslp=24/7/pi*sd/mu
! min/max of the radius distribution
  rmax=(n-1)*dx*rmax
  rmin=dx*rmin
! fractal parameter
  p=7./16.*moment/sd/(rmax-rmin)
! memory allocation
  allocate(r(na),pdf(m*n))
! fractal distribution of the radii (Zeng el al. 1994)
  do i=1,na
     r(i)=(2.*rand(0)*float(na)/p+rmax**(-2.))**(-.5)
  enddo
! Sort of the distribution by size
  call reorder(r,na)
  if (verb.eq.2) then 
     open(10,file='r.tmp')
     do i=1,na
        write(10,*) r(i)
     enddo
     close(10)
  endif
  if (verb.eq.1) write(0,*) 'rmax : ',r(1)
  if (verb.eq.1) write(0,*) 'rmin : ',r(na)
! locating the maximum of the pdf
  pdfmax=0.
  do i=1,m*n
     pdf(i)=fp(i)
     if (pdf(i) > pdfmax) then
        pdfmax=pdf(i)
        idxmax=i
     endif
     fp(i)=0.
  enddo
! loop over the asperities to place them on the fault
  imax=int(idxmax/n)+1
  jmax=idxmax-(imax-1)*n
  do i=1,na
     if ((mod(i,100).eq.0.or.i.eq.1).and.verb.eq.2)  write(0,*) 'radius #',i,r(i)
     lmin=int(r(i)/dx)+1
     lmax=m-int(r(i)/dx)-1
     wmin=int(r(i)/dx)+1
     wmax=n-int(r(i)/dx)-1
     if (i == 1) then
        lx=imax
        wx=jmax
     else
        curint=0.
        numrnd=rand(0)
        j=0
! draw of the asperity position
        do while (curint < numrnd)
           j=j+1
           if (j.eq.m*n) exit
           curint=curint+pdf(j)
        enddo
        lx=int(j/n)+1
        wx=j-(lx-1)*n
     endif
! adding the asperity to the slip distribution
     lx=min(lx,lmax)
     lx=max(lx,lmin)
     wx=min(wx,wmax)
     wx=max(wx,wmin)
     do j=(lx-int(r(i)/dx)),(lx+int(r(i)/dx))
        do k=(wx-int(r(i)/dx)),(wx+int(r(i)/dx))
           idx=k+(j-1)*n
           d=sqrt(((j-lx)*dx)**2.+((k-wx)*dx)**2.)
           if (d.lt.r(i)) then
              slp=cslp*sqrt(r(i)**2.-d**2.)
              fp(idx)=fp(idx)+slp
           endif
        enddo
     enddo
  enddo
! renormalization
  mf=0.
  do i=1,m*n
     mf=mf+fp(i)
  enddo
  mf=mf*dx**2.*mu
  if (verb.eq.1) write(0,*) 100*(mf-moment)/moment,'%'
  mg=0.
  do i=1,m*n
     fp(i)=fp(i)/mf*moment
     mg=mg+fp(i)
  enddo
  mg=mg/float(m*n)
  write(0,*) 'du medio : ',mg
  return
end subroutine pdfslipfract

!*******************************************************
subroutine reorder(a,n)
  implicit none

  integer n
  real*4 a(n)

  integer i
  real*4 mem
  logical done

  done=.false.
  do while (.not.done)
     done=.true.
     do i=1,n-1
        if (a(i).lt.a(i+1)) then
           mem=a(i)
           a(i)=a(i+1)
           a(i+1)=mem
           done=.false.
        endif
     enddo
  enddo
  return
end subroutine reorder
end module pdf2slip
