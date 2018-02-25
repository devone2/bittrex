#!/bin/python3
import subprocess
import cfscrape
import json
# With get_tokens() cookie dict:

# tokens, user_agent = cfscrape.get_tokens("http://somesite.com")
# cookie_arg = "cf_clearance=%s; __cfduid=%s" % (tokens["cf_clearance"], tokens["__cfduid"])

# With get_cookie_string() cookie header; recommended for curl and similar external applications:

cookie_arg, user_agent = cfscrape.get_cookie_string("https://bittrex.com")

# With a custom user-agent string you can optionally provide:

# ua = "Scraping Bot"
# cookie_arg, user_agent = cfscrape.get_cookie_string("http://somesite.com", user_agent=ua)

print("Cookie: " + cookie_arg)
print("User Agent: " + user_agent)

c = { "cookies": cookie_arg, "userAgent": user_agent }
f = open('cf.json','w')
json.dump(c,f)
f.close()

#result = subprocess.check_output(["curl", "--cookie", cookie_arg, "-A", user_agent, "http://somesite.com"])
