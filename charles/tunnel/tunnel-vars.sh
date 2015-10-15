# tunnel-var -- set up shell variables for a Layer 3 VPN tunnel over ssh.
# written by Charles Karney, http://charles.karney.info, 2007

# Edit this file and install as HOSTA:/root/bin/tunnel-vars and
# HOSTB:/root/bin/tunnel-vars (mode 644)

# See
#    http://charles.karney.info/tunnel/

# The IP address of the originating computer
HOSTA=1.2.3.4

# Its network and mask
ANET=1.2.0.0; AMASK=255.255.0.0

# The secondary IP address of the origination computer on the alternate
# interface.
HOSTAALT=1.2.3.5; ALTIF=eth1

# If non-null, another host on the same network from which regular ssh
# connections can be made for debugging purposes.
HOSTBACKUP=

# The number of the tunnel device on the originating computer.
TUNA=0

# The IP name (should not be the IP address unless this is static) of
# the destination computer.
HOSTB=hostb.home.net

# The IP address of the destination computer in the originating
# computer's network once the tunnel is set up.
HOSTBTUN=1.2.3.6

# The number of the tunnel device on the destination computer.
TUNB=0
