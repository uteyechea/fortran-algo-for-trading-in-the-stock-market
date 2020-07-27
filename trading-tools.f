! "Adama Maneuver," jumping in and out of the atmosphere in a freefall, is one example why you don't mess with humans. We're stone cold nuts.

!"A computer program is said to learn from experience E with respect to some class of tasks T and performance measure P, if its performance at tasks in T, as measured by P, improves with experience E."

!------------------------------------------------------------------------

Module Objects

    type t1
    Character(Len=17)::symbol !ticker symbol as listed in the exchange
    Character(Len=100)::name !company name as registered in
    Character(Len=3)::exchange
    Character(Len=30),Dimension(9)::exchangeName

    Integer::ndays !Total number of days with price data
    Real,allocatable::price(:,:)
    Real,Dimension(:,:),Allocatable::returns
    Integer,Dimension(:),Allocatable::date
    Integer,Dimension(:),Allocatable::volume

    real,allocatable::z(:,:) !This z variable hold all prices and returns normalized using feature scaling and standard deviation normalization

    real,allocatable::pdf(:) ! the probability density function can be estimated by dividing the domain of all data, [-1,1] in small segments and estimating the frequential probability definition for each small segment.
    
    real,allocatable::cor(:,:,:)
    
    end type

    !new type object for each method
    type,extends(t1)::t2

    integer id
    real,allocatable::y(:,:,:) !the sequence of predicted output
    real,allocatable::rar(:,:)

    end type

End Module

!------------------------------------------------------------------------

Module ProgramVariables

    use Objects

    integer Lookback
    integer allcompanies!keeps the count of The total numer of companies
    !SecuritiesExchange
    type(t1),dimension(:),allocatable::cmx
    type(t2),dimension(:,:),allocatable::output
    !other module

End Module

!------------------------------------------------------------------------

Module GlobalVariables

    use objects

    Integer,Parameter::MaxK=27, Bra=10, Ket=10
    !Character(Len=24),Parameter::Dir='C:/Users/abc/vyuex/Data/'
    !Character(Len=28),Parameter::Dir='/mnt/c/Users/abc/vyuex/Data/'
    Character(Len=27),Parameter::Dir='/mnt/c/vyuex-argonaut/Data/'
    Integer::ierr
    integer,parameter::methods=1 !Number of available algorithms

    !Main Global Constants
    Integer i,l,k,n,t !letters used in do cycles
    
    integer exchangeid
End Module

!------------------------------------------------------------------------

Module DataProcessing

     Contains


    Subroutine SecuritiesExchange(Exchange,AllCompanies,cmx)
    
    Use objects
    Use GlobalVariables
    


    Implicit None
    !Interface_var
    Integer,Intent(In)::Exchange
    Integer,Intent(Out)::allcompanies
    type(t1),dimension(:),allocatable,intent(inout)::cmx

    !Local_var_tmp
    Integer todayDateInt
    Integer,Dimension(8)::Values !
    Character(Len=8) todayDateChar !
    Character(Len=3)::StockExchange
    Character(Len=100),Dimension(1:2)::CompanyTmp
    Integer,Parameter::List1=2000

    Character(Len=17),Dimension(:),Allocatable::Company
    Character(Len=30),Dimension(9)::StockExchangeName

!----------------------------------------------------------
   Integer,Dimension(:,:),Allocatable::dates
   Real,Dimension(:,:,:),Allocatable::prices
   

    !Notes> Date is assigned to variable VALUES, the number date, the timestamp, will be another variable to process. Example> Every end of quarter somethng happens, this will be fed as another parameter inside gradientDescentFit.

!----------------------------------------------------------

    Select Case(Exchange); Case(1); StockExchange='NYX';Case(2); StockExchange='TSX'
    Case(3); StockExchange='LSE';Case(4); StockExchange='SGX';
    Case(5); StockExchange='HKX';Case(6); StockExchange='ASX';
    Case(7); StockExchange='SSX';Case(8); StockExchange='SZX';
    Case(9); StockExchange='XTR';Case(10); StockExchange='CCY';
    End Select

    StockExchangeName(1:9)=[&
    'New York, U.S.          ',&
    'Toronto, Canada         ',&
    'London, England         ', &
    'Singapore, Singapore    ',&
    'Hong Kong               ',&
    'Shanghai, China         ',&
    'Shenzhen, China         ',&
    'Frankfurt, Germany      ',&
    'FX                      ' &
    ]


    Call Date_and_time(Values=Values) !date_and_time(date,time,zone,values) Values(3)>Day Values(2)>Month Values(1)>Day


    !Do not know why all this is necessary if date will output char Date in CCYYMMDD, that you can read and write as done in (1)
    If (Values(3) > 9 .and. Values(2) > 9) Then
    Write(todayDateChar,'(I4,i2,i2)') Values(1),Values(2),Values(3)
    Else If (Values(3) <= 9 .and. Values(2) <= 9) Then
    Write(todayDateChar,'(I4,i0,i0,i0,i0)') Values(1),0,Values(2),0,Values(3)
    Else If (Values(3) > 9 .and. Values(2) <= 9) Then
    Write(todayDateChar,'(I4,i0,i0,i2)') Values(1),0,Values(2),Values(3)
    Else If (Values(3) <= 9 .and. Values(2) > 9) Then
    Write(todayDateChar,'(I4,i2,i0,i0)') Values(1),Values(2),0,Values(3)
    Endif


    Read(todayDateChar,'(i8)') TodayDateInt
    Write(*,'(A,1x,i0)') 'Local system date',todayDateInt


    write(*,*) 'Company list file path '//Dir//'Exchanges/'//StockExchange

    1001 continue
    !COUNT TOTAL NUMBER OF COMPANIES AND SAVE IT INSIDE VARIABLE "ALLCOMPANIES"
    !1000 continue
    Open(Unit=100,File=Dir//'Exchanges/'//StockExchange,Status='Old',IOSTAT = ierr); Rewind(100)
    IF (ierr /= 0) THEN             ! Windows probably hasn't released handle from a previous close command
    !CALL SLEEPQQ (500) ! Wait .5 seconds,
    !GO TO 1001                   ! then try again
    Write(*,*) "error opening file"
    stop
    END IF
    AllCompanies=0; Do; Read(100,'(A)',End=100); AllCompanies=AllCompanies+1; Enddo
    100 continue
    write(*,*) "The total number of companies is ", AllCompanies

!------------------------------
    allocate(cmx(allcompanies))
!------------------------------


    !SAVE THE NAME OF EACH COMPANY INSIDE THE CHARACTER TYPE VARIABLE CALLED "COMPANY" THAT HAS A FIXED LENGHT OF 17 SPACES
    !PROBLEM, FIXED LENGHT FOR CHARACTER TYPE VARIABLES CAN BE PRONE TO PROGRAM EXECUTION ERRORS
    Allocate(Company(AllCompanies)); Company=""
    Rewind(100); Do i=1,AllCompanies; Read(100,'(A)',End=101) Company(i); Enddo
    101 continue
    !Test
    !Write(*,*) "The first company is ", trim(company(1)), " and the last company is ", trim(company(allcompanies))
    close(100)
!------------------------------
    !Set ticker symbol inside company object. It seems to be the case that i cant read directly into the object the company symbol
    do i=1,allcompanies
        cmx(i)%symbol=company(i)
    enddo

    Write(*,*) "The first company is ", trim(cmx(1)%symbol), " and the last company is ", trim( cmx(allcompanies)%symbol )
!------------------------------


    !COUNT THE TOTAL NUMBER OF TRADING DAYS FOR EACH COMPANY, cmx%ndays
    Do i=1,AllCompanies
    Open(Unit=i+List1,File=Dir//'Equities/'//StockExchange//'/'//Trim(Company(i)),Status='Old',iostat=ierr) 
    IF (ierr /= 0) THEN; Write(*,*) "error opening file"; stop; END IF
    Rewind(i+List1)
        Do; Read(i+List1,'(I8)',End=10002); cmx(i)%ndays=cmx(i)%ndays+1;Enddo
    10002 close(i+list1)
    Enddo


    write(*,"(A,i0)") "Total number of days with price data for the first company are ", cmx(1)%ndays
    write(*,"(A,i0)") "Total number of days with price data for the last company are ", cmx(allcompanies)%ndays

    !------------------------------




    !DEFINE INTERNAL SUBROUTINE VARIABLES THAT WILL EXTRACT DATA FROM CSV FILES AND TRANSFER ALL DATA TO MEMORY
    Allocate(Dates(maxval(cmx(:)%ndays),AllCompanies)); dates=0
    Allocate(prices(6,maxval(cmx(:)%ndays),AllCompanies)); prices=0

    Do i=1,AllCompanies
        Open(Unit=i+List1,File=Dir//'Equities/'//StockExchange//'/'//Trim(Company(i)),Status='Old',IOSTAT = ierr)
        
        IF (ierr /= 0) THEN; Write(*,*) "error opening file"; stop; END IF

        Rewind(i+List1)
        Do l=1,cmx(i)%ndays
        Read(i+List1,*,end=10003) prices(1:6,l,i)
        Enddo

        10003 rewind(i+list1)

        Do l=1,cmx(i)%ndays
        Read(i+List1,'(I8)',end=10004) dates(l,i)
        Enddo

        10004 close(i+list1)
    Enddo


    write(*,*) "section1"

!//////////////////////////////////////////////
Do i=1,AllCompanies
    !write(*,*) i,"allocation started"
    allocate( cmx(i)%price(1:5,1:cmx(i)%ndays ))
    allocate( cmx(i)%date(1:cmx(i)%ndays ))
    allocate( cmx(i)%volume(1:cmx(i)%ndays ))
    allocate( cmx(i)%returns(1:5,1:cmx(i)%ndays ))
    !write(*,*) i,"allocation ended"
enddo
!/////////////////////////////////////////////

    Do i=1,AllCompanies

!if downloaded data is sorted in an asccending order
        !we are going to sort the data inside the files from oldest to newest and save the sorted data to memory
        Do l=cmx(i)%ndays,1,-1;
        cmx(i)%price(1:4,l)=prices(2:5,cmx(i)%ndays-l+1,i)   !(high,low,close,open) as defined in nyx.py
        !write(*,*) i,l,"price"
        cmx(i)%date(l)=dates(cmx(i)%ndays-l+1,i) !date in format yyyy/mm/dd
        !write(*,*) i,l,"date"
        cmx(i)%volume(l)=int(prices(6,cmx(i)%ndays-l+1,i)) !Trading volume
        !write(*,*) i,l,"volume"
        enddo

        Do l=cmx(i)%ndays,2,-1;
        cmx(i)%returns(1:6,l)=[(cmx(i)%price(1:5,l)/cmx(i)%price(1:5,l-1))-real(1),(cmx(i)%volume(l)/Real(cmx(i)%volume(l-1)+1))-real(1)];
        enddo
    enddo



        !------------------------------
    deallocate(dates,prices)


    write(*,*) "First company price data comparison "
    write(*,'(4(f0.2,2x))') cmx(1)%price(1:4,1)


    Open(Unit=101,File=Dir//'Lists/'//StockExchange//"tickers",Status='Unknown')
    Open(Unit=102,File=Dir//'Lists/'//StockExchange//"names",Status='Unknown')


    !PROBLEM IM NOT ABLE TO READ TICKERS AND NAMES INDEPENDENTLY
    !quick fix SOLUTION CREATE TWO FILES ONE WITH TICKERS AND ANOTHER WITH NAMES, THEY MUST HAVE THE SAME ORDER.

   
    Do i=1,AllCompanies
    Rewind(101);Rewind(102)
        Do
        Read(101,'(A)',End=201) CompanyTmp(1)
        Read(102,'(A50)',End=201) CompanyTmp(2)
        !write(*,*) "-",trim(CompanyTmp(1)),"-",trim(CompanyTmp(2)),"-",trim(company(i)),"-"
            If (Trim(CompanyTmp(1)) == Trim(Company(i)) ) Then
                cmx(i)%name=trim(CompanyTmp(2))
                go to 201
            Endif
        Enddo
    201 Continue
    Enddo

    write(*,*) "Testing company name for each ticker"
    write(*,*) "Company: symbol, name: ", trim(cmx(1)%symbol)," for ",trim(cmx(1)%name)
    write(*,*) "..."
    write(*,*) "Company: symbol, name: ", trim(cmx(allcompanies)%symbol)," for ",trim(cmx(allcompanies)%name)


    !deallocate(company) !for some unknown reason if i activate this all date in cmx%prices is lost

    End Subroutine








    Subroutine DataPreProcessing(AllCompanies,cmx) !FeatureScaling and Standarization

    use objects
    Use GlobalVariables

    Implicit None

    !Interface_var
    integer, intent(in)::allcompanies
    type(t1),dimension(allcompanies),intent(inout)::cmx

    write(*,*) "Test of passing of new type var argument between subroutines"
    Write(*,*) "The first company is ", trim(cmx(1)%symbol), " and the last company is ", trim( cmx(allcompanies)%symbol )



!/////////////////////////////////////////////
do i=1,allcompanies
    allocate(cmx(i)%z(maxk,cmx(i)%ndays))
enddo
write(*,*) "Allocation successful"
!/////////////////////////////////////////////


    !!$acc kernels
    Do i=1,AllCompanies
        Do l=cmx(i)%ndays,2,-1
            !SCALING FEATURES FOR DAILY PRICES [HIGH, LOW,CLOSE,OPEN,AVERAGE INTRADAY PRICE]
            Do k=1,5
                If ( Maxval(cmx(i)%price(k,1:l)) /= Minval(cmx(i)%price(k,1:l)) ) Then
                cmx(i)%z(k,l)=(cmx(i)%price(k,l)-( sum(cmx(i)%price(k,1:l))/max(real(1),real(size(cmx(i)%price(k,1:l)))) ) ) / (Maxval(cmx(i)%price(k,1:l))-Minval(cmx(i)%price(k,1:l)))
                Endif
            !STANDARIZATION OF DAILY PRICES
                If ( ( Sqrt( Sum((cmx(i)%price(k,1:l)-(Sum(cmx(i)%price(k,1:l))/Real(l)) )**2)) ) > 0 ) Then
                cmx(i)%z(k+5,l)=( cmx(i)%price(k,l)- (Sum(cmx(i)%price(k,1:l))/Real(l)) ) / ( Sqrt( Sum((cmx(i)%price(k,1:l)-(Sum(cmx(i)%price(k,1:l))/Real(l)) )**2)) )
                Endif
            Enddo
            !NON-ADJUSTED PRICES
            Do k=1,4
                cmx(i)%z(10+k,l)=cmx(i)%price(k,l)
            Enddo
            !SCALING FEATURES FOR DAILY returnsS
            Do k=1,6
                If ( Maxval(cmx(i)%returns(k,1:l)) /= Minval(cmx(i)%returns(k,1:l)) ) Then
                cmx(i)%z(k+15,l)=(cmx(i)%returns(k,l)- ( sum(cmx(i)%returns(k,1:l))/max(real(1),real(size(cmx(i)%returns(k,1:l))))) )/ (Maxval(cmx(i)%returns(k,1:l))-Minval(cmx(i)%returns(k,1:l)))
                endif
            !STANDARIZATION OF DAILY returnsS
                If ( ( Sqrt( Sum((cmx(i)%returns(k,1:l)-(Sum(cmx(i)%returns(k,1:l))/Real(l)) )**2)) ) > 0 ) Then
                cmx(i)%z(21+k,l)=( cmx(i)%returns(k,l)- (Sum(cmx(i)%returns(k,1:l))/Real(l)) ) / ( Sqrt( Sum((cmx(i)%returns(k,1:l)-(Sum(cmx(i)%returns(k,1:l))/Real(l)) )**2)) )
                Endif
            enddo
            !SCALING FEATURES FOR INTRDAY returnsS
            If (cmx(i)%price(4,l) > 0 .and. maxval((cmx(i)%price(3,1:l)/cmx(i)%price(4,1:l))-1.0) /= minval(((cmx(i)%price(3,1:l)/cmx(i)%price(4,1:l))-1.0)) ) Then
                cmx(i)%z(27,l)=( ((cmx(i)%price(3,l)/cmx(i)%price(4,l))-1.0) - ( sum(((cmx(i)%price(3,1:l)/cmx(i)%price(4,1:l))-1.0))/max(real(1),real(size(((cmx(i)%price(3,1:l)/cmx(i)%price(4,1:l))-1.0))))) ) / &
                                ( maxval((cmx(i)%price(3,1:l)/cmx(i)%price(4,1:l))-1.0) - minval(((cmx(i)%price(3,1:l)/cmx(i)%price(4,1:l))-1.0)) )
            endif
        Enddo
    Enddo
    !!$acc end kernels


write(*,*) "Data pre-processor finished"
!---------------------------------------------------

    End Subroutine



End Module




Module Algorithms; Contains



    subroutine pdf(allcompanies,cmx)

    use objects
    Use GlobalVariables
    
    Implicit None
    !Interface_var
    Integer,Intent(In)::allcompanies
    type(t1),dimension(allcompanies),intent(inout)::cmx
    !Local_var_tmp
    real,dimension(allcompanies,maxk)::mean

    !Find the parameters of each comapny data statistics
    !This will allow us to see if we can model the data using a normal standard distribution

    do i=1,allcompanies
        do k=1,maxk
    mean(i,k)= Sum(cmx(i)%z(k,1:cmx(i)%ndays)) / max(real(1),real(size(cmx(i)%z(k,1:cmx(i)%ndays)))) 
    !std(i)=
        enddo
    enddo    

    write(*,*) "Company processed data mean"
    do i=1,allcompanies
    write(*,"(A,1x,27(f0.3,1x))") trim(cmx(i)%symbol), mean(i,1:maxk)
    enddo

    end subroutine



    Subroutine Correlation(allcompanies,lookback,cmx) !assuming a uniform probability distribution

    use objects
    Use GlobalVariables
    
    Implicit None
    !Interface_var
    Integer,Intent(In)::allcompanies,lookback
    type(t1),dimension(allcompanies),intent(inout)::cmx

    !Local_var_tmp
    Integer ii
    integer,dimension(allcompanies)::day

!/////////////////////////////////////////////
    do i=1,allcompanies
        allocate(cmx(i)%cor(bra:ket,maxk,allcompanies))
        cmx(i)%cor(bra:ket,maxk,allcompanies)=0
    enddo
!/////////////////////////////////////////////

    Do i=1,AllCompanies
    !$acc kernels
        day(i)=cmx(i)%ndays-lookback
        Do ii=1,allcompanies
            day(ii)=cmx(ii)%ndays-lookback
            Do k=1,MaxK
                Do N=Bra,Ket 
    
    cmx(i)%cor(N,k,ii)=&

    Sum( &
    ( cmx(i)%z(k,day(i)-N:day(i)) - (Sum(cmx(i)%z(k,day(i)-N:day(i)))/max(real(1),real(size(cmx(i)%z(k,day(i)-N:day(i))))) ) ) &
    * &
    ( cmx(ii)%z(k,day(ii)-N:day(ii)) - (Sum(cmx(ii)%z(k,day(ii)-N:day(ii)))/max(real(1),real(size(cmx(ii)%z(k,day(ii)-N:day(ii))))) ) ) &
    ) / &

    ( Sqrt( Sum((&
    ( cmx(i)%z(k,day(i)-N:day(i)) - (Sum(cmx(i)%z(k,day(i)-N:day(i)))/max(real(1),real(size(cmx(i)%z(k,day(i)-N:day(i))))) ) ) &
    )**2)) * &
    Sqrt( Sum((&
    ( cmx(ii)%z(k,day(ii)-N:day(ii)) - (Sum(cmx(ii)%z(k,day(ii)-N:day(ii)))/max(real(1),real(size(cmx(ii)%z(k,day(ii)-N:day(ii))))) ) ) &
    )**2)) )
                enddo
            enddo
        enddo
    enddo            

    End Subroutine







    Subroutine Autocorrelation(AllCompanies,cmx,lookback,output)

    use objects
    use GlobalVariables

    Implicit None
    
    !Interface_var
    Integer,Intent(In)::AllCompanies,lookback
    type(t1),dimension(allcompanies),intent(in)::cmx
    type(t2),dimension(:,:),allocatable,intent(inout)::output

    !Local_var_tmp
    Real,Dimension(:,:,:),allocatable::R
    Real,Dimension(0:Ket,MaxK,Bra:Ket)::a
    integer,dimension(maxk,bra:ket)::counter
    integer,dimension(allcompanies)::day
   
    integer method
    real,parameter::minimum_positive_correlation=0.85


!-----------------------------------------------



    !--------------------------------------------------------------------

    a=0
    counter=0
    method=1

!/////////////////////////////////////////////
    allocate( output(method,allcompanies) )
!/////////////////////////////////////////////    

    Do i=1,AllCompanies
    !$acc kernels
        day(i)=cmx(i)%ndays-lookback

        Allocate(R(maxk,bra:ket,cmx(i)%ndays)); R=0

        Do t=cmx(i)%ndays,Ket+1,-1
            Do N=Bra,Ket
                Do k=1,MaxK
                    If (Sum( (cmx(i)%z(k,day(i)-N:day(i))-Sum(cmx(i)%z(k,day(i)-N:day(i)))/Real(N+1))**2) > 0.0 &
                    .and.  Sum( (cmx(i)%z(k,t-N:t)-Sum(cmx(i)%z(k,t-N:t))/Real(N+1))**2 ) > 0.0) Then
                    R(k,N,t)=&
                    (  Sum( ( (cmx(i)%z(k,day(i)-N:day(i))-Sum(cmx(i)%z(k,day(i)-N:day(i)))/Real(N+1))*&
                    (cmx(i)%z(k,t-N:t)-Sum(cmx(i)%z(k,t-N:t))/Real(N+1)) ) ) )/&
                    ( Sqrt(Sum( (cmx(i)%z(k,day(i)-N:day(i))-Sum(cmx(i)%z(k,day(i)-N:day(i)))/Real(N+1))**2 )) *&
                    Sqrt( Sum( (cmx(i)%z(k,t-N:t)-Sum(cmx(i)%z(k,t-N:t))/Real(N+1))**2 )) )
                    Endif
                Enddo
            Enddo
        Enddo


    !Identifies what past data is relevant to build the estimated future sequence.
   
        Do t=cmx(i)%ndays,Ket+1,-1
            Do k=1,MaxK
                Do N=Bra,Ket
                    if (R(k,N,t) >= minimum_positive_correlation ) then
                        if (k<11 .or. k>14) then
                            a(0:N,k,N)=a(0:N,k,N) + cmx(i)%z(k,t:t+N)
                            !Whats better to normalize or average vector a? Averaging data will not allow for
                            !proper comparison of different a vectors. No, its backwards, with normalized data
                            !i will always have a coordinate/dimension 1 in each a vector. In contrast, while
                            !averageging i will have days whose value will surpass 1 by a lot or not and that
                            !will SAY SOMETHING ABOUT THE CERTAINTY OF THE PRICE ACTION (UP/DOWN).
                            counter(k,N)=counter(k,N)+1
                        else
                            a(0:N,k,N)=a(0:N,k,N) + (cmx(i)%z(k,t:t+N)-(sum(cmx(i)%z(k,t:t+N))/max(real(1),real(size(cmx(i)%z(k,t:t+N))))))/(maxval(cmx(i)%z(k,t:t+N)-minval(cmx(i)%z(k,t:t+N))))
                            counter(k,N)=counter(k,N)+1
                        endif
                    endif
                Enddo
            Enddo
        Enddo

!i think each a vector should have its own length, it is difficult to add in a meaninful way vectors of different length

!/////////////////////////////////////////////
        allocate(output(method,i)%y(0:N,N,Maxk) )
!/////////////////////////////////////////////

        write(*,*) "allocation of output",i
        Do N=Bra,Ket    
            !allocate( cmx(i)%price(1:5,1:cmx(i)%ndays ))
            Do k=1,MaxK
            output(method,i)%y(0:N,N,k) = a(0:N,k,N) / max(real(1),real(counter(k,N)))
            enddo
        enddo

        deallocate(R)
    !$acc end kernels        
    enddo


    !--------------------------------------------------------------------





    End Subroutine


    subroutine raroc(allcompanies,output,cmx)
    
    use objects
    use GlobalVariables

    Implicit None
    
    !Interface_var
    Integer,Intent(In)::AllCompanies
    type(t2),dimension(:,:),allocatable,intent(inout)::output
    type(t1),dimension(allcompanies),intent(in)::cmx

    integer method
    real::forecast(allcompanies)

!--------------------------------------------------------------------
    method=1 !when more methods are developed i will need to cycle over all methods

!--------------------------------------------------------------------

    !ror= expected return / standard deviation

    !a very simplistic expected return with equal probabilities means the average of all values of x_i
    Do i=1,AllCompanies
    !/////////////////////////////////////////////
        allocate(output(method,i)%rar(Bra:ket,Maxk) )
    !/////////////////////////////////////////////
        Do N=Bra,Ket    
            !allocate( cmx(i)%price(1:5,1:cmx(i)%ndays ))
            Do k=1,MaxK

            output(method,i)%rar(N,k)= &
            (sum(output(method,i)%y(0:N,N,k)) / max(real(1),real(size(output(method,i)%y(0:N,N,k)))) ) / &
            ( Sqrt( Sum((&
            ( output(method,i)%y(0:N,N,k) - (Sum(output(method,i)%y(0:N,N,k))/max(real(1),real(size(output(method,i)%y(0:N,N,k)))) ) ) &
            )**2)) )
            enddo
        enddo
    enddo        


    forecast(i)=sum(output(method,i)%rar)


    do l=1,5
    write(*,*) "Long ideas"
    write(*,*) maxloc(forecast(:)), cmx(maxloc(forecast(:)))%symbol, cmx(maxloc(forecast(:)))%name
    forecast(maxloc(forecast(:)))=0
    write(*,*) "Short ideas"
    write(*,*) minloc(forecast(:)), cmx(minloc(forecast(:)))%symbol, cmx(minloc(forecast(:)))%name
    forecast(minloc(forecast(:)))=0

    enddo

     


    endsubroutine

End Module
!------------------------------------------------------------------------






!------------------------------------------------------------------------


PROGRAM system

use objects
Use GlobalVariables
Use DataProcessing
use Algorithms
use ProgramVariables

!Load External Modules
!The main program needs all arguments that are passed between subroutines defined in its own section
!that is we need a variable definition section for the main program and for each subroutine




IMPLICIT NONE

Write(*,*) "Select Exchange"
Write(*,*) "1) New York"
Write(*,*) "2) Toronto(TSX)"
Write(*,*) "3) London(LSE)"
Write(*,*) "4) Singapore(SGX)"
Write(*,*) "5) Hong Kong(HKeX)"
Write(*,*) "6) Sidney(ASX)"
Write(*,*) "7) Shanghai(SSE)"
Write(*,*) "8) Shenzhen(SZSE)"
Write(*,*) "9) Frankfurt (Xetra)"
Write(*,*) "10) Forex"
! Write(*,*) "10) Frankfurt"


Read(*,*) exchangeid


Write(*,*) 'Starting'

!111 Continue
call SecuritiesExchange(exchangeid,allCompanies,cmx)

!-------------------REMOVE FOR FULL FUNCTINOALITY
AllCompanies=50
!-----------------------------------------------


Write(*,*) 'Dynamic Feature Scaling and Standarization of Data, maps to [-1,1]'
Call DataPreProcessing(AllCompanies,cmx) !normalization of data using different methods

Do Lookback=0,0,-1

Write(*,'(A,1x,i0)') 'Last known Data Date', Maxval(cmx(1)%date(:))


!THE CORRELATION COEFFICIENT IS ONLY USED TO GENERATE REPORTS, WE SHOULD INLCUDE THE
!CORRELATION COEFFICIENT TO FIND A BETTER FORECAST.
!Call CorrelationMatrix(AllCompanies,MaxL,Search,Z,Cr)

write(*,*) "executing autocorrelation"
Call Autocorrelation(AllCompanies,cmx,lookback,output)
write(*,*) "executing correlation"
call Correlation(allcompanies,lookback,cmx)
write(*,*) "executing pdf"
call pdf(allcompanies,cmx)

Write(*,*) "Starting Services"

call raroc(allcompanies,output,cmx)

Enddo


END PROGRAM


!strats
!Sell calls before dividends
!Perfect the selling of premium, since we already know it works now lets find when it works the best



!todo
!StaticStablePrice will be made using standarization https://en.wikipedia.org/wiki/Feature_scaling look for standarization
!include as data higher than third order differentiation numerical formulas




!Notes: Add cr to support prediction

    !Note: Estimate convergence Ratio of Cross Company Correlation what companies are moving closer together?
