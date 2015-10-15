#! /bin/sh

# tunnelb -- script to configure HOSTB's side of a Layer 3 VPN tunnel over ssh.
# written by Charles Karney, http://charles.karney.info, 2007

# Install this as HOSTB:/root/bin/tunnelb (mode 755)

# See
#    http://charles.karney.info/tunnel/

# Read in variables
. /root/bin/tunnel-vars

# Rotate log files
LOG=/root/tunnelb.log
test -s $LOG.3 && mv -f $LOG.3 $LOG.4
test -s $LOG.2 && mv -f $LOG.2 $LOG.3
test -s $LOG.1 && mv -f $LOG.1 $LOG.2
test -s $LOG   && mv -f $LOG   $LOG.1
exec > $LOG 2>&1

# Wait a little for the tunnel to be set up
sleep 5

echo Initiate tunnel connection at `date -Is`

# Tunnel interface
TNBIF=tun$TUNB

# Diagnostic info before setup
ifconfig $TNBIF; route

# Determine default gateway and interface
set -- `route -n | grep '^0\.0\.0\.0 '`
GW=$2; PUBIF=$8

# Specify the endpoints of the tunnel
ifconfig $TNBIF $HOSTBTUN pointopoint $HOSTA

# Ensure that the original (tunneling) ssh connection uses the regular
# route and interface.
route add -host $HOSTAALT gw $GW $PUBIF

# If HOSTBACKUP is non-null, it uses the regular route too
test "$HOSTBACKUP" && route add -host $HOSTBACKUP gw $GW $PUBIF

# Otherwise all traffic destined for ANET uses the tunnel interface.
route add -net $ANET netmask $AMASK gw $HOSTBTUN $TNBIF

echo Tunnel connection set up at `date -Is`

# Diagnostic info after setup
ifconfig $TNBIF; route

# Note that no attempt is made to unconfigure the network when the
# tunnel shuts down.  On reconnecting some commands are run redundantly;
# but this is innocuous.
