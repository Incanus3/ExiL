#!/usr/bin/ruby

def load_package_file(filename) # array<array<string>>
  lines = []
  IO.foreach(filename) {|line| lines << line.split}
  lines
end

def load_defs_from_file(filename) # array<string>
  output = `egrep '^[^;]*(defun|defmethod|defmacro|defclass|defvar|defgeneric)' #{filename} \
    | awk '{ print $2 }' | grep '^[[:alpha:]*]'`
  accessors = `egrep -o '^[^;]*\;?' #{filename} | egrep -o ':(reader|writer|accessor) [^[:space:])]*' | egrep -o ' .*'`
  env_acc = `grep -o '\(exil-env-[^)]*\)' #{filename} | grep -v ',' | grep -o ' [^)]*'`
  output.split("\n") - ['print-object','initialize-instance'] + accessors.split + env_acc.chomp.split
end

def load_packages_to_use(packfile)
  packages = `awk '{print $1}' #{packfile}`
  packages.split[0..-2] # use the names of first n - 1 packages to use in the last one
end

def make_import_line(packname,symbols) # string
  str_packname = "exil#{packname.empty? ? "" : "-"}#{packname}"
  "(:shadowing-import-from :#{str_packname} #{symbols.map {|sym| "#{str_packname}::#{sym}"}.join(' ')})"
end

def make_package_definition(packname,import_lines,packages_to_use) # multiline string
  out = "(defpackage :#{packname}\n"
  out += "  (:use :common-lisp #{packages_to_use.map {|pack| ":exil-#{pack}"}.join(' ')})\n"
  out += "  " + import_lines.join("\n  ")
  out += ")\n"
end

def split_line_to_length(line,length = 80) # array<string>
  return [line] if line.length <= length
  lines = []
  curr_line = ""
  line.split.each do |word|
    if curr_line.empty?
      curr_line += word
    elsif curr_line.length + word.length < length
      curr_line += ' ' + word
    else
      lines << curr_line
      curr_line = word
    end
  end
  lines << curr_line
end

def run(package_file) # multiline string
  packages = load_package_file(package_file)
  use_packs = load_packages_to_use(package_file)
  import_lines = packages.map do |package|
    packname = package.shift().delete('"')
    definitions = []
    package.each do |filename|
      definitions += load_defs_from_file(filename)
    end
    split_line_to_length(make_import_line(packname,definitions.uniq),78)
  end
  make_package_definition("exil-test",import_lines,use_packs)
end

if __FILE__ == $0
  package_file = ARGV[0] || "packages.txt"
  package_definition = run(package_file)
  puts ';;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;'
  puts package_definition
  puts ';;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;'
end
