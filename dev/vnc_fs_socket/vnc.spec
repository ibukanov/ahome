%define xorg_version 6.8.1-4

Summary:   A remote display system.
Name:      vnc
Version:   4.0
Release: 8.2
URL:       http://www.realvnc.com
Source0:   http://www.realvnc.com/dist/vnc-4.0-unixsrc.tar.gz
Source1:   http://www.realvnc.com/dist/vnc-4.0-javasrc.tar.gz
# This is from 'rpmbuild -bp xorg-x11.spec':
Source2:   xorg-x11-%{xorg_version}.tar.bz2
Source3:   vncserver.init
Source4:   vncconfig.py
Source5:   vnc-16x16.png
Source6:   vnc-24x24.png
Source7:   vnc-48x48.png
Patch0:    vnc-cookie.patch
Patch1:    vnc-fPIC.patch
Patch2:    vnc-use-fb.patch
Patch3:    vnc-xclients.patch
Patch4:    vnc-idle.patch
Patch5:    vnc-via.patch
Patch8:    vnc-restart.patch
Patch9:    vnc-gcc34.patch
Patch10:   vnc-def.patch
Patch11:   vnc-xorg.patch
Patch12:   vnc-sparc.patch
Patch13:   vnc-pipe.patch
License:   GPL
Group:     User Interface/Desktops
BuildRoot: %{_tmppath}/%{name}-%{version}-root
BuildPrereq: /usr/bin/perl tcp_wrappers
BuildRequires: zlib-devel libjpeg-devel XFree86-devel
BuildRequires: desktop-file-utils >= 0.2.92
BuildRequires: gcc-java
Prereq:    /sbin/chkconfig /sbin/service
Requires: /sbin/runuser

%description
Virtual Network Computing (VNC) is a remote display system which
allows you to view a computing 'desktop' environment not only on the
machine where it is running, but from anywhere on the Internet and
from a wide variety of machine architectures.  This package contains a
client which will allow you to connect to other desktops running a VNC
server.

%package server
Summary: A VNC server.
Requires: bash >= 2.0, /usr/bin/mcookie
Group: User Interface/X
Prereq: /sbin/chkconfig /etc/init.d

%description server
The VNC system allows you to access the same desktop from a wide
variety of platforms.  This package is a VNC server, allowing others
to access the desktop on your machine.

%prep
%setup -q -n vnc-%{version}-unixsrc -a1
bzip2 -dc %{SOURCE2} | tar -xkf - || :
%patch0 -p1 -b .cookie
%patch1 -p1 -b .fPIC
%patch2 -p1 -b .use-fb
%patch3 -p1 -b .xclients
%patch4 -p1 -b .idle
%patch5 -p1 -b .via
%patch8 -p1 -b .restart
%patch9 -p1 -b .gcc34
%patch10 -p1 -b .def
%patch11 -p1 -b .xorg
%patch12 -p1 -b .sparc
%patch13 -p1

# Apply the patch from the VNC tarball.
filterdiff -x'*/cfbglblt8.c' xc.patch | patch -p0
#patch -p0 -i xc.patch

rm -rf java

%build
autoconf # For use-fb patch
%configure --with-installed-zlib --with-fb
make
mkdir -p unix/java
pushd vnc-%{version}-javasrc
make JAVAC="gcj -C"
popd

cd xc
make CDEBUGFLAGS="$RPM_OPT_FLAGS" CXXDEBUGFLAGS="$RPM_OPT_FLAGS" World FAST=1

%install
rm -rf %{buildroot}

bin=%{buildroot}%{_bindir}
man=%{buildroot}%{_mandir}
ext=%{buildroot}/usr/X11R6/%{_lib}/modules/extensions
mkdir -p $bin $man $man/man1 $ext
./vncinstall $bin $man $ext
chmod u+w $bin/* $ext/* || :

mkdir -p %{buildroot}%{_datadir}/vnc/classes
cp -aR unix/java/* %{buildroot}%{_datadir}/vnc/classes

mkdir -p %{buildroot}/etc/rc.d/init.d
install -m755 %{SOURCE3} %{buildroot}/etc/rc.d/init.d/vncserver

# vncconfig(1) front-end
install -m755 %{SOURCE4} %{buildroot}%{_bindir}/vncconfig.py

mkdir -p %{buildroot}/etc/sysconfig
cat > %{buildroot}/etc/sysconfig/vncservers << EOF
# The VNCSERVERS variable is a list of display:user pairs.
#
# Uncomment the line below to start a VNC server on display :1
# as my 'myusername' (adjust this to your own).  You will also
# need to set a VNC password; run 'man vncpasswd' to see how
# to do that.  
#
# DO NOT RUN THIS SERVICE if your local area network is
# untrusted!  For a secure way of using VNC, see
# <URL:http://www.uk.research.att.com/vnc/sshvnc.html>.

# VNCSERVERS="1:myusername"
# VNCSERVERARGS[1]="-geometry 800x600"
EOF
chmod 644 %{buildroot}/etc/sysconfig/vncservers

mkdir -p %{buildroot}%{_datadir}/icons/hicolor/{16x16,24x24,48x48}/apps
install %{SOURCE5} %{buildroot}%{_datadir}/icons/hicolor/16x16/apps/vnc.png
install %{SOURCE6} %{buildroot}%{_datadir}/icons/hicolor/24x24/apps/vnc.png
install %{SOURCE7} %{buildroot}%{_datadir}/icons/hicolor/48x48/apps/vnc.png
mkdir -p %{buildroot}/etc/X11/applnk/Applications
cat > vncviewer.desktop << EOF
[Desktop Entry]
Encoding=UTF-8
Name=VNC Viewer
Comment=VNC client application
Exec=/usr/bin/vncviewer
Icon=vnc.png
Terminal=false
Type=Application
StartupWMClass=vncviewer
EOF
# Desktop file installation.
mkdir $RPM_BUILD_ROOT%{_datadir}/applications
desktop-file-install --vendor vnc --delete-original    \
  --dir $RPM_BUILD_ROOT%{_datadir}/applications        \
  --add-category X-Red-Hat-Extra                       \
  --add-category Utility                               \
  --add-category Application                           \
  vncviewer.desktop

%clean
rm -rf %{buildroot}

%post server
if [ "$1" = 1 ]; then
  /sbin/chkconfig --add vncserver
fi

%preun server
if [ "$1" = 0 ]; then
  /sbin/service vncserver stop >/dev/null 2>&1
  /sbin/chkconfig --del vncserver
fi

%postun server
if [ "$1" -ge "1" ]; then
  /sbin/service vncserver condrestart >/dev/null 2>&1
fi

%files
%defattr(-,root,root)
%doc LICENCE.TXT README 
%{_bindir}/vncviewer
%{_datadir}/applications/*
%{_mandir}/man1/vncviewer.1*
%{_datadir}/icons/hicolor/*/*/*

%files server
%defattr(-,root,root)
%attr(0755,root,root) %config /etc/rc.d/init.d/vncserver
%config(noreplace) /etc/sysconfig/vncservers
%{_bindir}/Xvnc
%{_bindir}/vncpasswd
%{_bindir}/vncconfig
%{_bindir}/vncconfig.py
%{_bindir}/vncserver
%{_bindir}/x0vncserver
%{_datadir}/vnc
%{_mandir}/man1/Xvnc.1*
%{_mandir}/man1/vncpasswd.1*
%{_mandir}/man1/vncconfig.1*
%{_mandir}/man1/vncserver.1*
%{_mandir}/man1/x0vncserver.1*
/usr/X11R6

%changelog
* Wed Oct  6 2004 Tim Waugh <twaugh@redhat.com> 4.0-8
- Use runuser not su in initscript (bug #134594).

* Wed Sep 29 2004 Tim Waugh <twaugh@redhat.com> 4.0-7
- Upgraded base package to xorg-x11-6.8.1-4.

* Tue Aug 31 2004 Tim Waugh <twaugh@redhat.com> 4.0-6
- Upgraded base package to xorg-x11-6.7.99.903-1.

* Fri Aug 27 2004 Tim Waugh <twaugh@redhat.com> 4.0-5
- Built for Fedora Core 2.

* Wed Aug 25 2004 Tim Waugh <twaugh@redhat.com> 4.0-4
- Apply and enable Kristian Høgsberg's --use-fb patch.

* Mon Aug  2 2004 Tim Waugh <twaugh@redhat.com>
- Fixed vnc-via patch (bug #128940).

* Thu Jun 24 2004 Tim Waugh <twaugh@redhat.com> 4.0-3
- 4.0.
- No longer need hotspot patch.
- Add sparc patch from bug #126382.

* Tue Jun 15 2004 Elliot Lee <sopwith@redhat.com>
- rebuilt

* Tue Jun  1 2004 Tim Waugh <twaugh@redhat.com> 4.0-1.beta5.6
- Turn ppc64 builds on again.

* Tue Jun  1 2004 Tim Waugh <twaugh@redhat.com> 4.0-1.beta5.5
- Exclude ppc64 until the build machine is fixed.
- Undo last vnc.def change to get vnc.so back.

* Fri May 28 2004 Tim Waugh <twaugh@redhat.com> 4.0-1.beta5.4
- Further vnc.def fix.
- Fix cursor handling for hotspots outside the bounding rectangle.

* Thu May 27 2004 Tim Waugh <twaugh@redhat.com> 4.0-1.beta5.3
- Fix ppc64 build.
- Fix debuginfo package.
- Another fix for REGION_INIT usage.

* Wed May 26 2004 Tim Waugh <twaugh@redhat.com> 4.0-1.beta5.2
- Switch to xorg-x11 (bug #119530).

* Thu May 20 2004 Tim Waugh <twaugh@redhat.com> 4.0-1.beta5.1
- 4.0beta5.
- Removed compat, f8 and crash patches.
- Fixed via patch now that NULL is not a valid parameter default.
- Updated gcc34 patch.

* Wed Apr 14 2004 Tim Waugh <twaugh@redhat.com> 4.0-1.beta4.11
- Allow parameters to be specified in the init script (bug #60176).

* Wed Mar 31 2004 Tim Waugh <twaugh@redhat.com> 4.0-1.beta4.10
- Back down to XFree86 again, since the Xvnc binary in 4.0-1.beta4.9 doesn't
  work at all.

* Tue Mar 23 2004 Tim Waugh <twaugh@redhat.com> 4.0-1.beta4.9
- Build against xorg-x11.

* Tue Mar 02 2004 Elliot Lee <sopwith@redhat.com>
- rebuilt

* Fri Feb 13 2004 Elliot Lee <sopwith@redhat.com>
- rebuilt

* Sun Feb  1 2004 Tim Waugh <twaugh@redhat.com>
- Build fixes for GCC 3.4.

* Fri Jan 23 2004 Tim Waugh <twaugh@redhat.com> 4.0-0.beta4.8
- Fix last fix (bug #114063).

* Thu Jan 22 2004 Tim Waugh <twaugh@redhat.com> 4.0-0.beta4.7
- Restart connect() if a signal interrupts it (bug #114063).

* Thu Nov 27 2003 Tim Waugh <twaugh@redhat.com>
- Fixed -via parsing bug in vncviewer (bug #110919).

* Fri Nov 14 2003 Tim Waugh <twaugh@redhat.com> 4.0-0.beta4.6
- F8 fix (bug #109377).

* Mon Nov 10 2003 Tim Waugh <twaugh@redhat.com> 4.0-0.beta4.5
- Better fix for bug #107455.

* Sat Nov  8 2003 Tim Waugh <twaugh@redhat.com> 4.0-0.beta4.4
- Fixed zero-sized cursor problem (bug #107455).

* Thu Oct  2 2003 Tim Waugh <twaugh@redhat.com>
- Added icons (bug #105820).

* Fri Sep 19 2003 Tim Waugh <twaugh@redhat.com> 4.0-0.beta4.3
- Avoid a potential Xvnc crash (bug #104702).

* Fri Sep  5 2003 Tim Waugh <twaugh@redhat.com> 4.0-0.beta4.2
- 4.0beta4.
- No longer need compile, dotvnc, norender patches.
- Don't be so picky about which architectures to build on any more.
- Build requires XFree86-devel.

* Thu Aug 28 2003 Tim Waugh <twaugh@redhat.com> 4.0-0.beta3.10
- Fixed SSH tunnelling support (bug #102213).

* Fri Aug 22 2003 Tim Waugh <twaugh@redhat.com> 4.0-0.beta3.9
- Add SSH tunnelling support (bug #102213).

* Wed Aug 20 2003 Tim Waugh <twaugh@redhat.com> 4.0-0.beta3.8
- Disable IdleTimeout by default.

* Mon Aug 18 2003 Tim Waugh <twaugh@redhat.com> 4.0-0.beta3.7
- Ship vncconfig(1) front-end.
- In default xstartup, exec /etc/vnc/xstartup if it exists.
- Track XFree86 package.
- Fix compilation (bug #102547).
- Fix vncpasswd (bug #102435).

* Tue Aug 12 2003 Tim Waugh <twaugh@redhat.com> 4.0-0.beta3.6
- Use patched tarball from XFree86 package.
- Build Java VNC viewer.

* Wed Aug  6 2003 Tim Waugh <twaugh@redhat.com> 4.0-0.beta3.5.1
- Rebuilt.

* Wed Aug  6 2003 Tim Waugh <twaugh@redhat.com> 4.0-0.beta3.5
- Make default xclients file include instructions for 'normal' desktop.
- Server subpackage doesn't require XFree86.
- desktop file: add StartupWMClass tag
- init script: set USER (bug #101674).
- init script: pause when restarting to give the server a chance to stop
  (bug #84817).
- vncpasswd: create ~/.vnc if it doesn't already exist.

* Tue Aug  5 2003 Tim Waugh <twaugh@redhat.com> 4.0-0.beta3.4
- Allow old -dontdisconnect option, and behave a bit more like Xvnc used to
  if it is given (default to no password, use RFB3.3 protocol).
- Fixed DisconnectClients bug.

* Mon Aug  4 2003 Tim Waugh <twaugh@redhat.com> 4.0-0.beta3.3
- Disable RENDER altogether for Xvnc.

* Mon Aug  4 2003 Tim Waugh <twaugh@redhat.com> 4.0-0.beta3.2
- Reinstated init script.
- Add work-around for Xcursor bug.

* Thu Jul 31 2003 Tim Waugh <twaugh@redhat.com> 4.0-0.beta3.1
- Initial package.
