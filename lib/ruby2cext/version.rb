
module Ruby2CExtension

	VERSION = "0.2.0"

	FULL_VERSION_STRING = "Ruby2CExtension #{VERSION} (ruby #{RUBY_VERSION} (#{RUBY_RELEASE_DATE}) [#{RUBY_PLATFORM}])"

	SUPPORTED_RUBY_VERSIONS = {
		"1.8.4" => ["2005-12-24"],
		"1.8.5" => ["2006-08-25", "2006-12-04", "2006-12-25", "2007-03-13", "2007-06-07"],
		"1.8.6" => ["2007-03-13", "2007-06-07"],
	}

	if (dates = SUPPORTED_RUBY_VERSIONS[RUBY_VERSION])
		unless dates.include? RUBY_RELEASE_DATE
			warn "Ruby2CExtension warning: ruby #{RUBY_VERSION} (#{RUBY_RELEASE_DATE}) is an unknown release of Ruby #{RUBY_VERSION}, there might be problems"
		end
	else
		warn "Ruby2CExtension warning: ruby #{RUBY_VERSION} (#{RUBY_RELEASE_DATE}) is not supported by Ruby2CExtension #{VERSION}"
	end

end
