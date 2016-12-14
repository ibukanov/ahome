function FindProxyForURL(url, host)
{
    var ssh_socks_proxy = "SOCKS 127.0.0.1:7010";
    var ip = myIpAddress();

    // Check for DNB local net
    if (isInNet(ip, "10.196.0.0", "255.255.0.0"))
	return ssh_socks_proxy;
	
    if (isPlainHostName(host)) {
	switch (host) {
	case "watson":
	    return ssh_socks_proxy;
	}
    }

    return "DIRECT";
}
