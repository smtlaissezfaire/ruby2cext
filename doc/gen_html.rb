require "redcloth"

Dir.chdir(File.dirname(__FILE__))

template = File.read("html_template")

Dir["*.txt"].each { |fn|
	txt = File.read(fn)
	title = txt[/^h1. (.+?)$/, 1]
	txt.gsub!(/^PRE\n/m, "<pre><code>")
	txt.gsub!(/^PREEND$/, "</code></pre>")
	cnt = 0
	sections = []
	txt.gsub!(/^h2\.\s(.+)$/) { sections << $1; "h2(#section#{cnt+=1}). #$1" }
	unless sections.empty?
		sect_list = "\nSections: "
		cnt = 0
		sect_list << sections.map { |s| "\"#{s}\":#section#{cnt+=1}" }.join(", ")
		sect_list << ".\n\n"
		txt.sub!(/\nh2\(/m, sect_list << "h2(")
	end
	html = RedCloth.new(txt).to_html
	File.open("#{File.basename(fn, ".txt")}.html", "w") { |f|
		f.puts(template % [title, html])
	}
}
