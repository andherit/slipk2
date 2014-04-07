! This program generates a k2 dislocation distribution based
! on a summation of asperities, with a fractal distribution
! of the radius (Zeng et al, bssa, 1994).
! An input file is needed. The order of the parameter inside
! this file is not important. See the exemple slip.inp
! (Messina like source ...)
! Usage :
! slipk2 input=filename
! The ouput is created in a tmp directory (you need to create it).
! The p.tmp file is the probability fonction on the fault and
! the d.tmp is the dislocation. The normalization may be done
! on a stress drop basis or on a total moment basis.
! If the gaussian number in the input file is null, The code
! will expect an existing p.tmp file.
! For any question : herrero@ingv.it
!******************************************************************
! 14/04/07 Version 2.0.0 (Fortran90 version)
program slipk2
  implicit none

  integer :: m,n,ng,seed,i,na,sct
  real*4, dimension(:,:), allocatable :: gauss
  real*4, dimension(:), allocatable :: fp
  real*4 :: dx,sd,moment,mu,rmin,rmax,sd1
      character*1 dvd

! If you want to change to divider character (for example ' ')
! for the gaussian values, change the following variable
  dvd=';'
  call parsefile(m,n,dx,sd,ng,gauss,mu,na,rmin,rmax,seed,moment,sct,dvd)
  allocate(fp(m*n))
  if (ng /= 0) then
     write(0,'(a,$)') 'computing pdf ...'
     call faultpdf(gauss,ng,fp,m,n,dx)
     open(10,file='tmp/p.tmp',form='unformatted',access='direct',recl=4*m*n)
     write(10,rec=1) (fp(i),i=1,m*n)
  else
     write(0,'(a,$)') 'loading pdf ...'
     open(10,file='tmp/p.tmp',form='unformatted',access='direct',recl=4*m*n,status='old')
     read(10,rec=1) (fp(i),i=1,m*n)
  endif
  close(10)
  write(0,*) ' done!'
  write(0,'(a,$)') 'computing slip ...'
  call pdfslipfract(fp,m,n,dx,seed,moment,sd,mu,na,rmin,rmax,1,sct)
  write(0,*) ' done!'
  write(0,*) 'writing ...'
  open(10,file='tmp/d.tmp',form='unformatted',access='direct',recl=4*m*n)
  write(10,rec=1) (fp(i),i=1,m*n)
  close(10)
end program slipk2

!***********************************************************
subroutine faultpdf(gauss,ng,fp,m,n,dx)
  implicit none
      
!  read a list of ng 2D gaussian defined by their
!  center (xi,yi), their sigma (sigi) iand their weight (wgti)
!  to construct on the fault plane, a pdf compatible with
!  the subroutine pdfslipfract.
!  The fault plane is defined by its nodes through an array
!  fp(m,n) of dimension (m-1)dx*(n-1)dx

   integer :: m,n,ng
   real*4, dimension(ng,4) :: gauss
   real*4, dimension(m*n) :: fp
   real*4 :: dx

   real*4, parameter :: pi=acos(-1.)
   integer i,j,k,idx
   real*4 xl,yl,dst2,unormgauss,fpint

   fp=0.
   fpint=0.
   do i=1,m
      xl=(i-1)*dx
      do j=1,n
         yl=(j-1)*dx
         idx=j+(i-1)*n
         do k=1,ng
            dst2=(yl-gauss(k,2))**2.+(xl-gauss(k,1))**2.
            unormgauss=exp(-dst2/2./gauss(k,3)**2.)
            fp(idx)=fp(idx)+unormgauss*gauss(k,4)
         enddo
         fpint=fpint+fp(idx)
      enddo
   enddo
   do i=1,m*n
      fp(i)=fp(i)/fpint
   enddo  
   return
end subroutine faultpdf

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

!*********************************************************
subroutine parsefile(m,n,dx,sd,ng,gauss,mu,na,rmin,rmax,seed,m0,sct,dvd)
  implicit none

  integer m,n,ng,na,seed,sct
  real*4 dx,sd,mu,rmin,rmax,m0
  real*4, dimension(:,:), allocatable :: gauss
  character*1 dvd

  integer i,j
  real*4 val4(4)
  logical fparsei,fparsef,parsec,fparsearrf
  character*4 s4
  character*50 fileparam

  if (.not.parsec('input',fileparam)) stop 'fault file name missing'
  if (.not.fparsei(fileparam,'nx',m)) stop 'nx missing'
  if (.not.fparsei(fileparam,'ny',n)) stop 'ny missing'
  if (.not.fparsef(fileparam,'dx',dx)) stop 'dx missing'
  if (.not.fparsef(fileparam,'stress drop',sd)) then
     if (.not.fparsef(fileparam,'moment',m0)) then
        stop 'scaling parameters are missing'
     else
        sct=0
     endif
  else
     if (.not.fparsef(fileparam,'moment',m0)) then
        sct=1
     else
        stop 'both scaling parameters are exclusive'
     endif
  endif
  if (.not.fparsef(fileparam,'rigidity',mu)) stop 'rigidity missing'
  if (.not.fparsei(fileparam,'nb cracks',na)) stop 'crak number missing'
  if (.not.fparsef(fileparam,'rmin',rmin)) stop 'minimum radius missing'
  if (.not.fparsef(fileparam,'rmax',rmax)) stop 'maximum radius missing'
  if (.not.fparsei(fileparam,'seed',seed)) stop 'seed number missing'
  if (.not.fparsei(fileparam,'gaussian number',ng)) stop 'ng missing'
  if (ng.eq.0) return
  allocate(gauss(ng,4))
  do i=1,ng
! safe up to 99. need to be improved with a log calculation
     if (i < 10) then
        s4='[ ]'
        write(s4(2:2),'(i1.1)') i
     else
        s4='[  ]'
        write(s4(2:3),'(i2.2)') i
     endif
     if (.not.fparsearrf(fileparam,s4,val4,4,dvd)) then
        write(0,*) 'missing info for gaussian number#',i
        stop 
     else
        do j=1,4
           gauss(i,j)=val4(j)
        enddo
     endif
  enddo
  return
end subroutine parsefile

