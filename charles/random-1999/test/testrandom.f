      program testrandom
      implicit none
      include 'random.h'

* The seed for the RNG
      integer seed(ran_s)

* And its "decimal" representation.
      character*(ran_c) string

* ran_index and ran_array hold the "state" of the random number generator
* DON'T CHANGE THEM!
      integer ran_index
      double precision ran_array(ran_k)

* Declare random
      double precision random
      external random
      
      double precision x,xa(1234)

      print '(a)', 'Test of random number routines'
      print '(a)', 'All numbers should match EXACTLY'

      call decimal_to_seed('3.141592653589793238462643383279502',seed)

      call next_seed3(23,-95,110,seed)
      call seed_to_decimal(seed,string)
      print '(a,a)','Seed (2902248648199272781830143864736810): ',string

      call random_init(seed,ran_index,ran_array)
      
      x=random(ran_index,ran_array)
      print '(a,f17.15)','Zeroth number (0.540373998032440): ',x

      call random_array(xa,1234,ran_index,ran_array)
      print '(a,f17.15)','1234th number (0.478292786832260): ',xa(1234)

      call next_seed3(-23,95,-110,seed)
      call seed_to_decimal(seed,string)
      print '(a,a)','Seed (3141592653589793238462643383279502): ',string
      
      stop
      end
