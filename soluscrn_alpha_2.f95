program soluscrn_alpha_2

	implicit none
	
	! Declare variables
	
	integer :: ierror = 1 ! Set default iostat error to 1, i.e., ERROR, a 0 is no error
	
	integer :: row, col ! Need this data from input file to allocate array sizes
	integer :: i, j ! Need this to loop
	
	character(len=20) :: infile ! Will have this inputted from command line
	character(len=20) :: outfile ! Will have this in FIRST line of input file, just like MODFLOW
	
	character(len=8),allocatable,dimension(:) :: sol ! Solute ID
	character(len=8),allocatable,dimension(:) :: uni ! Solute units
	
	real,allocatable,dimension(:,:) :: Cm ! Concentrations measured
	real,allocatable,dimension(:,:) :: Ce ! Concentrations expected
	real,allocatable,dimension(:,:) :: RFt ! Recovery factor time step
	real,allocatable,dimension(:) :: RFi ! Recovery factor integrated

	character(len=20),dimension(29) :: str1 ! String format for character output
	character(len=20),dimension(29) :: str2 ! String format for real output
	character(len=20),dimension(29) :: str3 ! String format for character output without time column
	character(len=20),dimension(29) :: str4 ! String format for real output without time column
	
	! New addition to V2: table of correlation co-efficients (r) and co-effecients of determination (r^2)
	! but total new variables is 6, sumx, sumxsq, sumxy, r, and rsq
	! Also, have new array for "rxn" table for ease of data analysis visualization
	
	real,allocatable,dimension(:) :: sumx, sumxsq, oldval1D ! Explain...
	real,allocatable,dimension(:,:) :: sumxy, r, rsq, oldval2D ! Explain...
	integer :: k ! Explain...
	character(len=8),allocatable,dimension(:,:) :: rchar, rsqchar, corrchar ! Explain...
	real :: rmin,rsqmin ! Explain...
	
	! Get name of input file from command line
	
	write(*,*) ""
	write(*,*) "		========SOLUSCRN-23 V2 ALPHA========"
	write(*,*) "		----------NSF  Uranium Grant----------"
	write(*,*) "		************Award# 2229869************"
	write(*,*) "		.............UW-Milwaukee............."
	write(*,*) ""
	
	write(*,*) "Enter name of input file with extenstion e.g., input.txt:"
	read(*,*) infile

	! Open input file with data
	
	open(unit=10, file=infile, status="old", iostat=ierror)
	
	write(*,*) "" !Skip a line in command line
	
	if (ierror==0) then
		write(*,*) "Input file opened? Yes :)"
	else 
		write(*,*) "Input file opened? No :("	
	end if
	
	! Read in outfile, row, and col
	
	read(10,*,iostat=ierror) outfile, row, col 
	
	if (ierror==0) then
		write(*,*) "Input file read for output file, row, and column? Yes :)"
	else	
		write(*,*) "Input file read for output file, row, and column? No :("
	end if

	! Allocate to fill array with explicit dummy values
	
	allocate(Cm(row,col))
	Cm(:,:)=1.
	
	allocate(Ce(row-2,col))
	Ce(:,:)=1.
	
	allocate(RFt(row-3,col))
	RFt(:,:)=1.
	
	allocate(RFi(col))
	RFi(:)=1.
	
	allocate(sol(col))
	sol(:)="Hi"
	
	allocate(uni(col))
	uni(:)="Bye"
	
	! New additions to V2, i.e., 6 new variables...j is 6th new variable and is non-allocatable
	! oldval1D and oldval2D aren't really variables, they are more like placeholders for sum loops
	! Also, have new array for "corrchar" table for ease of data analysis visualization
	
	allocate(sumxy(col,col))
	sumxy(:,:)=0.
	
	allocate(r(col,col))
	r(:,:)=0.
	
	allocate(rsq(col,col))
	rsq(:,:)=0.

	allocate(oldval2D(col,col))
	oldval2D(:,:)=0.

	allocate(sumx(col))
	sumx(:)=0.
	
	allocate(sumxsq(col))
	sumxsq(:)=0.
	
	allocate(oldval1D(col))
	oldval1D(:)=0.
	
	allocate(rchar(col+1,col+1))
	rchar(:,:)="NULL"
	
	allocate(rsqchar(col+1,col+1))
	rsqchar(:,:)="NULL"
	
	allocate(corrchar(col+1,col+1))
	corrchar(:,:)="NULL"
	
	! Fill up (read in) sol and uni with character data	
	
	rewind(10) ! Start reading from top/beginning of input file
	read(10,*) ! Skip output file name
	read(10,*) ! Skip row size
	read(10,*) ! Skip column size
	read(10,*,iostat=ierror) sol,uni
	
	sol(:)=adjustr(sol(:)) ! Right adjust character data
	uni(:)=adjustr(uni(:)) ! Right adjust character data
	
	corrchar(1,2:col+1)=sol(:) ! Fill up top row, 2nd column and beyond, with solute name
	corrchar(2:col+1,1)=sol(:) ! Fill up first column, 2nd row and beyond, with solute name

	rchar(1,2:col+1)=sol(:) ! Fill up top row, 2nd column and beyond, with solute name
	rchar(2:col+1,1)=sol(:) ! Fill up first column, 2nd row and beyond, with solute name
	
	rsqchar(1,2:col+1)=sol(:) ! Fill up top row, 2nd column and beyond, with solute name
	rsqchar(2:col+1,1)=sol(:) ! Fill up first column, 2nd row and beyond, with solute name
	
	if (ierror==0) then
		write(*,*) "Input file read for solute ID and units? Yes :)"
	else	
		write(*,*) "Input file read for solute ID and units? No :("
	end if
	
	! Fill up (read in) Cm with real data
	
	rewind(10) ! Start reading from top/beginning of input file
	read(10,*) ! Skip output file name
	read(10,*) ! Skip row size
	read(10,*) ! Skip column size
	read(10,*) ! Skip solute ID
	read(10,*) ! Skip solute units
	
	do i=1,row
		read(10,*,iostat=ierror) Cm(i,1:col)
	end do
	
	if (ierror==0) then
		write(*,*) "Input file read for measured solute concentrations? Yes :)"
	else	
		write(*,*) "Input file read for measured solute concentrations? No :("
	end if
	
	!Calculate Ce, see this paper for equation: https://ngwa.onlinelibrary.wiley.com/doi/10.1111/gwat.12770 

	do j=2,col
		do i=1,row-2
			Ce(i,j)=((Cm(i+2,2)-Cm(2,2))/(Cm(1,2)-Cm(2,2)))*(Cm(1,j)-Cm(2,j))+Cm(2,j)
		end do
	end do

	! Don't forget about time column...
	
	do i=1,row-2
		Ce(i,1)=Cm(i+2,1)
	end do

	! Calculate recovery factor (RFt) at each time step, this is numerical integeration using the trapezoid rule

	! Don't forget about time column FIRST...time step is in between i and i+1...
	
	do i=1,row-3
		RFt(i,1)=(Cm(i+2,1)+Cm(i+3,1))/2.
	end do

	do j=2,col
		do i=1,row-3
			RFt(i,j)=((Cm(i+3,1)-Cm(i+2,1))*(Cm(i+3,j)+Cm(i+2,j))/2.)&
			/((Ce(i+1,1)-Ce(i,1))*(Ce(i+1,j)+Ce(i,j))/2.)
		end do
	end do
	
	! Calculate recovery factor (RFi) integrated at end, trapezoid rule again here...
	
	do j=2,col
		RFi(j)=sum(RFt(1:row-3,j))/sum(RFt(1:row-3,2)) !CHECK THAT ROW-3 IS OKAY
	end do
		
	! Exectute new additions to V2 below, break into 5 parts
	
	! Part 1: sumx
	
	oldval1D(:) = 0.
	do j = 1,col
		do i=1,row-3
			sumx(j)=RFt(i,j)+oldval1D(j)
			oldval1D(j) = sumx(j)
		end do
	end do
	
	!write(*,'(f5.2)') (sumx(i),i=1,col)
	
	! Part 2: sumxsq
	
	oldval1D(:) = 0.	
	do j = 1,col
		do i=1,row-3
			sumxsq(j)=(RFt(i,j))**2.+oldval1D(j)
			oldval1D(j) = sumxsq(j)
		end do
	end do
	
	!write(*,'(f6.2)') (sumxsq(i),i=1,col)
	
	! Part 3: sumxy
	
	oldval2D(:,:) = 0.
	do k = 1,col
		do j = 1,col
			do i=1,row-3
				sumxy(k,j)=RFt(i,k)*RFt(i,j)+oldval2D(k,j) ! This loop to me a while to figure out...:(
				oldval2D(k,j) = sumxy(k,j)
			end do
		end do
	end do
	
	! Part 4: r
	
	do i = 1,col
		do j = 1,col
			r(i,j)=(sumxy(i,j)-((sumx(i)*sumx(j))/(row-3)))&
			/(((sumxsq(i)-(sumx(i)**2.)/(row-3))*(sumxsq(j)-(sumx(j)**2.)/(row-3)))**(1/2.)) ! This loop to me a while to figure out...:(
		end do
	end do
	
	! Part 5: rsq
	
	rsq(:,:)=r(:,:)**2. ! Easy enough to do
	
	! Part 6: +corr, -corr, no corr table
	
	! Let's cook down these r and r^2 tables to a table that points the scientist to where the action is
	! Ask user for rmin and rsqmin
	
	write(*,*) "" !Skip a line in command line
	write(*,*) "Enter values for minimum correlation co-efficient (r) and co-efficient of determination (r^2) e.g., 0.75, 0.50:"
	read(*,*,iostat=ierror) rmin, rsqmin
	
	if (ierror==0) then
		write(*,*) "SOLUSCRN-23 V2 ALPHA ran successfully: check directory for ouput file :)"
	else	
		write(*,*) "SOLUSCRN-23 V2 ALPHA failed :( write nasty e-mail to paradisc@uwm.edu"
	end if
	
		do j=1,col
			do i=1,col
				if (r(i,j) >= rmin .and. rsq(i,j) >= rsqmin) then ! Explain...
				corrchar(i+1,j+1)="+corr"
				else if (r(i,j) <= -rmin .and. rsq(i,j) >= rsqmin) then
				corrchar(i+1,j+1)="-corr"
				else
				corrchar(i+1,j+1)=""
				end if
			end do
		end do
	
	do i=1,col
		do j=1,col
			write(rchar(i+1,j+1),'(f5.2)') r(i,j) ! Explain...
		end do
	end do

	do i=1,col
		do j=1,col
			write(rsqchar(i+1,j+1),'(f5.2)') rsq(i,j) ! Explain...
		end do
	end do
	
	corrchar(1,1)="" ! Blank cell in upper left-hand corner of corrchar character array
	rchar(1,1)="" ! Blank cell in upper left-hand corner of rchar character array
	rsqchar(1,1)="" ! Blank cell in upper left-hand corner of rsqchar character array
	
	do i=2,col+1
		corrchar(i,i)="sym" ! Write "sym" for symmertry across diagonal of corrchar character array
		rchar(i,i)="sym" ! Write "sym" for symmertry across diagonal of rchar character array
		rsqchar(i,i)="sym" ! Write "sym" for symmertry across diagonal of rsqchar character array
	end do
	
	corrchar(:,:)=adjustr(corrchar(:,:)) ! Right adjust character data in corrchar array
	rchar(:,:)=adjustr(rchar(:,:)) ! Right adjust character data in rchar array
	rsqchar(:,:)=adjustr(rsqchar(:,:)) ! Right adjust character data in rsqchar array
	 
	! Open output file and fill with data and calcs
	
	open(unit=1000, file=outfile, iostat=ierror)
	
	! Get strings in order for formatting number of columns to output
	! Do character format first, this is 'a', then do real format, this is 'f'
	! Make use of ASCII character table that defines a 'character' value for integers, e.g., 48 is '0'
	! 49 is '1' and so on...
	! Max number of columns is 29, but this can be increased later
	
	do i=1,9
		str1(i)='('//'1x'//achar(i+48)//'a)'
		str2(i)='('//'1x'//achar(i+48)//'f8.2)'
		str3(i)='('//'9x'//achar(i+48)//'a)' ! Pad 9 spaces to the right to make up for lost time column
		str4(i)='('//'9x'//achar(i+48)//'f8.2)' ! Pad 9 spaces to the right to make up for lost time column
	end do
	
	do i=10,19
		str1(i)='('//'1x'//achar(49)//achar(i+38)//'a)'
		str2(i)='('//'1x'//achar(49)//achar(i+38)//'f8.2)'
		str3(i)='('//'9x'//achar(49)//achar(i+38)//'a)' ! Pad 9 spaces to the right to make up for lost time column
		str4(i)='('//'9x'//achar(49)//achar(i+38)//'f8.2)' ! Pad 9 spaces to the right to make up for lost time column
	end do
	
	do i=20,29
		str1(i)='('//'1x'//achar(50)//achar(i+28)//'a)'
		str2(i)='('//'1x'//achar(50)//achar(i+28)//'f8.2)'
		str3(i)='('//'9x'//achar(50)//achar(i+28)//'a)' ! Pad 9 spaces to the right to make up for lost time column
		str4(i)='('//'9x'//achar(50)//achar(i+28)//'f8.2)' ! Pad 9 spaces to the right to make up for lost time column
	end do
	
	! Write results to output file, make them line up right justified
	
	! Unable to figure out how to do this in format statement, but within write statement works okay...
	
	write(1000,*) "Concentrations Measured:"
	write(1000,(str1(col))) (sol(i),i=1,col)
	write(1000,(str1(col))) (uni(i),i=1,col)
	write(1000,(str2(col))) ((Cm(i,j),j=1,col),i=1,row)
	
	write(1000,*) "Concentrations Expected:"
	write(1000,(str1(col))) (sol(i),i=1,col)
	write(1000,(str1(col))) (uni(i),i=1,col)
	write(1000,(str2(col))) ((Ce(i,j),j=1,col),i=1,row-2)	
		
	write(1000,*) "Recovery Factors Temporal:"
	write(1000,(str1(col))) (sol(i),i=1,col)
	write(1000,(str2(col))) ((RFt(i,j),j=1,col),i=1,row-3)	
	
	write(1000,*) "Recovery Factors Integrated:"
	write(1000,(str3(col-1))) (sol(i),i=2,col)
	write(1000,(str4(col-1))) (RFi(i),i=2,col)

	write(1000,*) "Correlation Co-efficients (r):"
	write(1000,(str1(col+1))) ((rchar(i,j),j=1,col+1),i=1,col+1)
	!write(1000,(str1(col))) (sol(i),i=1,col)
	!write(1000,(str2(col))) ((r(i,j),j=1,col),i=1,col)	
	
	write(1000,*) "Co-efficients of Determination (r^2):"
	write(1000,(str1(col+1))) ((rsqchar(i,j),j=1,col+1),i=1,col+1)
	!write(1000,(str1(col))) (sol(i),i=1,col)
	!write(1000,(str2(col))) ((rsq(i,j),j=1,col),i=1,col)
	
	write(1000,*) "Correlation Summary Table:"
	write(1000,(str1(col+1))) ((corrchar(i,j),j=1,col+1),i=1,col+1)
	
	! Close input file and deallocate variables
	
	close(10)
	deallocate(Cm,Ce,RFt,RFi,sol,uni)
	deallocate(sumx, sumxsq, oldval1D)
	deallocate(sumxy, r, rsq, oldval2D)
	deallocate(rchar, rsqchar, corrchar)
	close(1000)

end program soluscrn_alpha_2