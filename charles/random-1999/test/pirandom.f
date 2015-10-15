      program pirandom

* Compute pi by counting the number of randomly generated point lying
* in a circle.  See Gropp et al., ``Using MPI'', Section 3.7.
      implicit none

* MPI declarations
      include 'mpif.h'
      integer id, ierr, len, numprocs
      character*(mpi_max_processor_name) hostname

* Declarations for random numbers
      include 'random.h'
      integer seed(ran_s)
      character*(ran_c) string
      integer ran_index
      double precision ran_array(ran_k)

* For |date_and_time| routine
      character cdate*8,ctime*10,czone*5
      integer time(8)

* A place to store random numbers
      integer m
      parameter (m=1000)
      double precision z(m)

* Other local vaiables
      double precision  pi
      integer iterations
      integer a,sum,tot,i,j

* Set iteration count (each iteration computes $m/2$ points)
      iterations = 10000

* Initialize MPI
      call mpi_init(ierr)
      call mpi_comm_rank(mpi_comm_world, id, ierr)
      call mpi_comm_size(mpi_comm_world, numprocs, ierr)

* Determine and print the seed
      if (id .eq. 0) then
         call date_and_time(cdate,ctime,czone,time)
         call set_random_seed(time,seed)
         call seed_to_decimal(seed,string)
         print *,'An inefficient way to compute pi via Monte Carlo'
         print *,'Seed is set to ',string
      endif

* Tell everyone the value of seed
      call mpi_bcast(seed,ran_s,mpi_integer,0,mpi_comm_world,ierr)

* Advance the seed according to the MPI process number
      call next_seed(id,seed)

* Initialize the random number generator
      call random_init(seed,ran_index,ran_array)
      
* Determine the number of points lying inside a circle of radius 1
      a=0
      do i=(iterations*id)/numprocs,(iterations*(id+1))/numprocs-1
         call random_array(z,m,ran_index,ran_array)
         do j=1,m,2
            if (z(j)**2 + z(j+1)**2 .lt. 1.0d0) then
               a=a+1
            end if
         end do
      end do

* Print some dignostic information
      call mpi_get_processor_name(hostname,len,ierr)
      print *,'Process ',id,' running on ',hostname(1:len),' gives ',a

* Assemble the results onto process 0
      call mpi_reduce(a,sum,1,mpi_integer,mpi_sum,0,mpi_comm_world,ierr)

* Node 0 prints the answer.
      tot=iterations*m/2
      if (id .eq. 0) then
         pi=(4.0d0*sum)/tot
         print *, 'pi is approximately ',4*sum,'/',tot,' = ',pi
      endif

* Terminate MPI
      call mpi_finalize(ierr)

      stop
      end
