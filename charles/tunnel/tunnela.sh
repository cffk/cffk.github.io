#! /bin/sh

# tunnela -- script to configure HOSTA's side of a Layer 3 VPN tunnel over ssh.

# written by Charles Karney, http://charles.karney.info, 2007

# Install this as HOSTA:/root/bin/tunnela (mode 755)

# See
#    http://charles.karney.info/tunnel/

# Read in variables
. /root/bin/tunnel-vars

# Rotate log files
LOG=/root/tunnela.log
test -s $LOG.3 && mv -f $LOG.3 $LOG.4
test -s $LOG.2 && mv -f $LOG.2 $LOG.3
test -s $LOG.1 && mv -f $LOG.1 $LOG.2
test -s $LOG   && mv -f $LOG   $LOG.1

# Put everything into the background, since the tunnel devices aren't
# set up until this script completes.
(
    # Wait a little for the tunnel to be set up
    sleep 5

    echo Initiate tunnel connection at `date -Is`

    # Determine the default interface
    PUBIF=`route -n | awk '/^0\.0\.0\.0 /{print $8;}'`

    # Tunnel interface
    TNAIF=tun$TUNA

    # Diagnostic info before setup
    ifconfig $TNAIF; arp

    # Specify the endpoints of the tunnel.
    ifconfig $TNAIF $HOSTA pointopoint $HOSTBTUN

    # Advertise HOSTA's MAC address as HOSTBTUN's
    arp -sD $HOSTBTUN $PUBIF pub

    echo Tunnel connection set up at `date -Is`

    # Diagnostic info after setup
    ifconfig $TNAIF; arp

    # Note that no attempt is made to unconfigure the network when the
    # tunnel shuts down.  On reconnecting some commands are run
    # redundantly; but this is innocuous.

) > $LOG 2>&1 < /dev/null &
