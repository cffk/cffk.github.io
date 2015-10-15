#! /bin/sh

# tunnel -- script to initiate Layer 3 VPN tunnel over ssh
# written by Charles Karney, http://charles.karney.info, 2007

# Install this as HOSTA:/root/bin/tunnel (mode 755)

# See
#    http://charles.karney.info/tunnel/

. /root/bin/tunnel-vars

# Rotate log files
LOG=/root/tunnel.log
test -s $LOG.3 && mv -f $LOG.3 $LOG.4
test -s $LOG.2 && mv -f $LOG.2 $LOG.3
test -s $LOG.1 && mv -f $LOG.1 $LOG.2
test -s $LOG   && mv -f $LOG   $LOG.1
exec > $LOG 2>&1

HOSTBIP=

while true; do

    # Need route connections to HOSTB through ALTIF.  So create a
    # routing rule for this whenever HOSTB's IP address changes.
    NEWHOSTBIP=`host $HOSTB | head -1 |
	grep "^$HOSTB has address " | cut -d' ' -f4`
    if test -z "$NEWHOSTBIP"; then
	echo No IP address for $HOSTB 1>&2
	sleep 15m
	continue
    fi
    if test "$NEWHOSTBIP" != "$HOSTBIP"; then
	# Remove outdated route
	test "$HOSTBIP" &&
	    route del -host $HOSTBIP $ALTIF
	HOSTBIP="$NEWHOSTBIP"
	# Add new route
	route add -host $HOSTBIP $ALTIF
    fi

    echo Connection starting at `date -Is`

    # Note on ssh flags:

    # -o StrictHostKeyChecking=yes
    #   prevents a connection being made to the "wrong" machine, i.e., a
    #   machine whose host key doesn't match that in .ssh/known_hosts.

    # -o PreferredAuthentications=publickey
    #   disallows prompting for password.

    # -o Compression=yes
    # 	It's best to compress the tunnel traffic.  Any ssh connections
    # 	made over the tunnel should be uncompressed.  This way we avoid
    # 	compressing already compressed data.  Non-ssh traffic benefits
    # 	from compression.  Latency is minimized by compressing at the
    # 	"outer" level.

    # -o TCPKeepAlive=yes
    # -o ServerAliveCountMax=10
    # -o ServerAliveInterval=15
    #   These ensure that the connection disappears in a timely fashion
    #   in case of network problems.  The script will then sleep for 5
    #   mins and attempt to reconnection.

    # -o Tunnel=point-to-point
    # -w $TUNA:$TUNB
    #   requests a Layer 3 point-to-point VPN tunnel on tun$TUNA and
    #   tun$TUNB.

    # -o PermitLocalCommand=yes
    # -o LocalCommand="/root/bin/tunnela"
    #   specifies a command to run on HOSTA's side.  This setup of the
    #   point-to-point endpoints and adds HOSTBTUN to the arp table.

    # -2
    #   Use protocol 2.  Protocol 1 doesn't support tunnels.

    # -x -a -n
    #   Minimize the overhead by turning off X forwarding, agent
    #   forwarding, and by not allocating a pseudo tty on the remote
    #   end.

    # ssh specifies to run true on HOSTB.  However HOSTB's
    # authorized_keys overrides this command (with command="...") and
    # runs /root/bin/tunnelb instead.  There is a possible race
    # condition here when HOSTB's IP address is changing.  ssh may uses
    # a different IP address from the one for which the route through
    # ALTIF is set up.  However this connection will be denied because
    # the connection will then be from HOSTA and HOSTB's authorized_keys
    # only allows access from HOSTAALT and not from HOSTA.  We have to
    # connect to HOSTB (the IP name) instead of the numeric IP address,
    # because otherwise whenever the IP address changes ssh won't be
    # able to verify that the ssh host key is valid (since this is
    # tagged to the IP name in known_hosts).

    ssh -o StrictHostKeyChecking=yes \
	-o PreferredAuthentications=publickey \
	-o Compression=yes \
	-o TCPKeepAlive=yes \
	-o ServerAliveCountMax=10 \
	-o ServerAliveInterval=15 \
	-o Tunnel=point-to-point \
	-o PermitLocalCommand=yes \
	-o LocalCommand="/root/bin/tunnela" \
	-2 -x -a -n -w $TUNA:$TUNB \
	$HOSTB true

    # Once the remote command has completed, ssh stays connected until
    # the tunnel disappears for whatever reason (network outage or a
    # machine going down).  In this case, this script waits for 5m and
    # tries to connect again.

    echo Connection ending at `date -Is`
    sleep 5m
done

exit
