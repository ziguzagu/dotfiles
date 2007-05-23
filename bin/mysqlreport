#!/usr/bin/perl -w

# mysqlreport v3.0a Dec 12 2006
# http://hackmysql.com/mysqlreport

# mysqlreport makes an easy-to-read report of important MySQL status values.
# Copyright (C) 2006  Daniel Nichter
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# The GNU General Public License is available at:
# http://www.gnu.org/copyleft/gpl.html

use strict;
use File::Temp qw(tempfile);
use DBI;
use Getopt::Long;
eval { require Term::ReadKey; };
my $RK = ($@ ? 0 : 1);

sub have_op;

my $WIN = ($^O eq 'MSWin32' ? 1 : 0);
my %op;
my %mycnf; # ~/.my.cnf
my ($tmpfile_fh, $tmpfile);
my ($stat_name, $stat_val, $stat_label);
my ($major, $minor, $patch, $x); # MySQL version
my (%stats, %vars); # SHOW STATUS, SHOW VARIABLES
my (%DMS_vals, %Com_vals, %ib_vals);
my ($dbh, $query);
my ($questions, $key_read_ratio, $key_write_ratio, $dms);
my ($key_cache_block_size, $key_buffer_used, $key_buffer_usage);
my ($qc_mem_used, $qc_hi_r, $qc_ip_r); # Query Cache
my ($need_myisam_vals, $need_innodb_vals);
my ($ib_bp_used, $ib_bp_total, $ib_bp_read_ratio);

GetOptions (
   \%op,
   "user=s",
   "password:s",
   "host=s",
   "port=s",
   "socket=s",
   "no-mycnf",
   "dtq|tdq|dq",
   "dms",
   "com:i",
   "sas",
   "tab",
   "qcache",
   "id-only|ido|innodb-only",
   "id|innodb",
   "dpr|drp",
   "all",
   "infile|in=s",
   "outfile=s",
   "flush-status",
   "email=s",
   "help|?"
);

show_help_and_exit() if $op{'help'};

get_user_mycnf() unless $op{'no-mycnf'};

# Command line options override ~/.my.cnf
$mycnf{'host'}   = $op{'host'}   if have_op 'host';
$mycnf{'port'}   = $op{'port'}   if have_op 'port';
$mycnf{'socket'} = $op{'socket'} if have_op 'socket'; 
$mycnf{'user'}   = $op{'user'}   if have_op 'user';

# Default values if nothing else
$mycnf{'host'}   ||= 'localhost';
$mycnf{'port'}   ||= 3306;
$mycnf{'socket'} ||= '/var/run/mysqld/mysqld.sock'; # Debian default
$mycnf{'user'}   ||= $ENV{'USER'};

if(exists $op{'password'})
{
   if($op{'password'} eq '') # Prompt for password
   {
      Term::ReadKey::ReadMode(2) if $RK;
      print "Password for database user $mycnf{'user'}: ";
      chomp($mycnf{'pass'} = <STDIN>);
      Term::ReadKey::ReadMode(0), print "\n" if $RK;
   }
   else { $mycnf{'pass'} = $op{'password'}; } # Use password given on command line
}

if($op{'all'} and not defined $op{'com'})  { $op{'com'} = 3; }
if(defined $op{'com'} and $op{'com'} == 0) { $op{'com'} = 3; }

# Connect to MySQL
if(!have_op 'infile')
{
   my $dsn;

   if(-S $mycnf{'socket'} && !have_op 'host') { $dsn = "DBI:mysql:mysql_socket=$mycnf{socket}";         }
   else                                       { $dsn = "DBI:mysql:host=$mycnf{host};port=$mycnf{port}"; }

   $dbh = DBI->connect($dsn, $mycnf{'user'}, $mycnf{'pass'}) or die;
}

# The report is written to a tmp file first.
# Later it will be moved to $op{'outfile'} or emailed $op{'email'} if needed.
($tmpfile_fh, $tmpfile) = tempfile() or die("Can't open temporary file for writing: $!\n");

# Determine if we need MyISAM, InnoDB, or both vals, then
# get vals and system vars from MySQL or infile, then
# set vals for our own vars
which_vals();
get_vals_vars();
set_myisam_vals() if $need_myisam_vals;
set_ib_vals()     if $need_innodb_vals;


# Write the reports
select $tmpfile_fh;
$~ = 'MYSQL_TIME', write;
if($need_myisam_vals)
{
   $~ = 'KEY_BUFF_MAX', write;
   if($key_buffer_usage != -1)     { $~ = 'KEY_BUFF_USAGE', write }
   $~ = 'KEY_RATIOS', write;
   if($op{'dtq'}    or $op{'all'}) { write_DTQ(); }
   $~ = 'SLOW_DMS', write;
   if($op{'dms'}    or $op{'all'}) { write_DMS(); }
   if($op{'com'}    or $op{'all'}) { write_Com(); }
   if($op{'sas'}    or $op{'all'}) { $~ = 'SAS', write; }
   if($op{'qcache'} or $op{'all'}) { write_qcache(); }
   $~ = 'REPORT_END', write;
   if($op{'tab'}    or $op{'all'}) { $~ = 'TAB', write; }
}
write_InnoDB() if $need_innodb_vals;
close $tmpfile_fh and select STDOUT;

email_report($tmpfile) if have_op 'email';

cat_report($tmpfile);

if(have_op 'outfile')
{
   if($WIN) { `move $tmpfile $op{outfile}`; }
   else     { `mv $tmpfile $op{outfile}`;   }
}
else
{
   if($WIN) { `del $tmpfile`;   }
   else     { `rm -f $tmpfile`; }
}

if(!have_op 'infile')
{
   if($op{'flush-status'})
   {
      $query = $dbh->prepare("FLUSH STATUS;");
      $query->execute();
   }

   $query->finish();
   $dbh->disconnect();
}

exit;

#
# Subroutines
#
sub show_help_and_exit
{
   print <<"HELP";
mysqlreport v3.0a Dec 12 2006
mysqlreport makes an easy-to-read report of important MySQL status values.

Command line options (abbreviations work):
   --user USER     Connect to MySQL as USER
   --password PASS Use PASS or prompt for MySQL user's password
   --host ADDRESS  Connect to MySQL at ADDRESS
   --port PORT     Connect to MySQL at PORT
   --socket SOCKET Connect to MySQL at SOCKET
   --no-mycnf      Don't read ~/.my.cnf
   --dtq           Show Distribution of Total Questions
   --dms           Show DMS details
   --com N         Show top N number of non-DMS questions
   --sas           Show SELECT and Sort report
   --tab           Show Thread, Aborts, and Bytes reports
   --qcache        Show Query Cache report
   --innodb        Show InnoDB report
   --innodb-only   Show only InnoDB report (hide all other reports)
   --dpr           Show Data, Pages, Rows report in InnoDB report
   --all           Show --dms --com 3 --sas --qcache --innodb --dpr
   --infile FILE   Read status values from FILE instead of MySQL
   --outfile FILE  Write report to FILE
   --email ADDRESS Email report to ADDRESS (doesn't work on Windows)
   --flush-status  Issue FLUSH STATUS; after getting current values
   --help          Prints this

Visit http://hackmysql.com/mysqlreport for more information.
HELP

   exit;
}

sub get_user_mycnf
{
   return if $WIN;
   open MYCNF, "$ENV{HOME}/.my.cnf" or return;
   while(<MYCNF>) { $mycnf{$1} = $2 if /^(.+?)\s*=\s*"?(.+?)"?\s*$/; }
   $mycnf{'pass'} ||= $mycnf{'password'} if exists $mycnf{'password'};
   close MYCNF;
}

sub which_vals
{
   $need_myisam_vals = 1;
   $need_innodb_vals = 1; # This could be set to 0 later in get_vals_vars()

   $need_myisam_vals = 0, return if have_op 'id-only';
   $need_innodb_vals = 0, return if (!$op{'id'} && !$op{'id-only'} && !$op{'all'});
}

sub get_vals_vars
{
   if(!have_op 'infile')
   {
      my @row;

      get_MySQL_version();

      # Get status values
      if($major >= 5 && (($minor == 0 && $patch >= 2) || $minor > 0)) {
         $query = $dbh->prepare("SHOW GLOBAL STATUS;");
      }
      else {
         $query = $dbh->prepare("SHOW STATUS;");
         $need_innodb_vals = 0;
      }
      $query->execute();
      while(@row = $query->fetchrow_array()) { $stats{$row[0]} = $row[1]; }

      # Get server system variables
      $query = $dbh->prepare("SHOW VARIABLES;");
      $query->execute();
      while(@row = $query->fetchrow_array()) { $vars{$row[0]} = $row[1]; }
   }
   else
   {
      # Default values if not set in INFILE
      $vars{'version'} = "0.0.0";
      $vars{'table_cache'} = 64;
      $vars{'max_connections'} = 100;
      # key_cache_block_size
      $vars{'key_buffer_size'} = 8388600; # 8M
      # query_cache_size
      $vars{'thread_cache_size'} = 0;

      open INFILE, "< $op{infile}" or die;
      while(<INFILE>)
      {
         next if /^\+\-/;
         next if /^$/;

         chomp;

         if(/([A-Za-z_]+)[\s|]+(\d+)/)
         {
            $stats{$1} = $2;
            next; 
         }

         # Explicit var = val (e.g. key_buffer_size = 128M)
         $vars{$1} = ($3 ? $2 * 1024 * 1024 : $2) and next if(/^\s*(\w+)\s*=\s*([0-9.]+)(M*)\s*$/);
         
         # print "Unrecognized line in infile: $_\n";
      }
      close INFILE;

      get_MySQL_version();
   }
}

sub get_MySQL_version
{
   if(have_op 'infile')
   {
      ($major, $minor, $patch) = ($vars{'version'} =~ /(\d{1,2})\.(\d{1,2})\.(\d{1,2})/);
   }
   else
   {
      my @row;

      $query = $dbh->prepare("SHOW VARIABLES LIKE 'version';");
      $query->execute();
      @row = $query->fetchrow_array();
      ($major, $minor, $patch) = ($row[1] =~ /(\d{1,2})\.(\d{1,2})\.(\d{1,2})/);
   }
}

sub set_myisam_vals
{
   $questions = $stats{'Questions'};

   $key_read_ratio = sprintf "%.3f",
                     ($stats{'Key_read_requests'} ?
                      $stats{'Key_reads'} / $stats{'Key_read_requests'} :
                      0);

   $key_write_ratio = sprintf "%.3f",
                      ($stats{'Key_write_requests'} ?
                       $stats{'Key_writes'} / $stats{'Key_write_requests'} :
                       0);

   $key_cache_block_size = (defined $vars{'key_cache_block_size'} ?
                            $vars{'key_cache_block_size'} :
                            1024);

   $key_buffer_used = $stats{'Key_blocks_used'} * $key_cache_block_size;

   if(defined $stats{'Key_blocks_unused'}) { # MySQL 4.1.2+
      $key_buffer_usage =  $vars{'key_buffer_size'} -
                           ($stats{'Key_blocks_unused'} * $key_cache_block_size);
   }
   else { $key_buffer_usage = -1; }

   # Data Manipulation Statements: http://dev.mysql.com/doc/refman/5.0/en/data-manipulation.html
   %DMS_vals = (
      SELECT  => $stats{'Com_select'},
      INSERT  => $stats{'Com_insert'}  + $stats{'Com_insert_select'},
      REPLACE => $stats{'Com_replace'} + $stats{'Com_replace_select'},
      UPDATE  => $stats{'Com_update'}  +
                 (exists $stats{'Com_update_multi'} ? $stats{'Com_update_multi'} : 0),
      DELETE  => $stats{'Com_delete'}  +
                 (exists $stats{'Com_delete_multi'} ? $stats{'Com_delete_multi'} : 0)
   );

   $dms = $DMS_vals{SELECT} + $DMS_vals{INSERT} + $DMS_vals{REPLACE} + $DMS_vals{UPDATE} + $DMS_vals{DELETE};
}

sub set_ib_vals
{
   $ib_bp_used  = ($stats{'Innodb_buffer_pool_pages_total'} -
                   $stats{'Innodb_buffer_pool_pages_free'}) *
                   $stats{'Innodb_page_size'};

   $ib_bp_total = $stats{'Innodb_buffer_pool_pages_total'} * $stats{'Innodb_page_size'};

   $ib_bp_read_ratio = sprintf "%.3f",
                       ($stats{'Innodb_buffer_pool_read_requests'} ?
                        $stats{'Innodb_buffer_pool_reads'} / $stats{'Innodb_buffer_pool_read_requests'} :
                        0);
}

sub sec_to_dhms # Seconds to days hours:minutes:seconds
{
   my $s = shift;
   my ($d, $h, $m) = (0, 0, 0);

   return '0 0:0:0' if $s <= 0;

   if($s >= 86400)
   {
      $d = int $s / 86400;
      $s -= $d * 86400;
   }

   if($s >= 3600)
   {
     $h = int $s / 3600;
     $s -= $h * 3600;
   }
   
   $m = int $s / 60;
   $s -= $m * 60;
   
   return "$d $h:$m:$s";
}

sub make_short
{
   my ($number, $kb, $d) = @_;
   my $n = 0;
   my $short;

   $d ||= 2;

   if($kb) { while ($number > 1023) { $number /= 1024; $n++; }; }
   else { while ($number > 999) { $number /= 1000; $n++; }; }

   $short = sprintf "%.${d}f%s", $number, ('','k','M','G','T')[$n];
   if($short =~ /^(.+)\.(00)$/) { return $1; } # 12.00 -> 12 but not 12.00k -> 12k

   return $short;
}

sub perc # Percentage
{
   my($is, $of) = @_;
   return sprintf "%.2f", ($is * 100) / ($of ||= 1);
}

sub t # Time average per second
{
   my $val = shift;
   return 0 if !$val;
   return(make_short($val / $stats{'Uptime'}, 0, 1));
}

sub email_report # Email given report to $op{'email'}
{
   return if $WIN;

   my $report = shift;

   open SENDMAIL, "|/usr/sbin/sendmail -t";
   print SENDMAIL "From: mysqlreport\n";
   print SENDMAIL "To: $op{email}\n";
   print SENDMAIL "Subject: MySQL status report on $mycnf{'host'}\n\n";
   print SENDMAIL `cat $report`;
   close SENDMAIL;
}

sub cat_report # Print given report to screen
{
   my $report = shift;
   my @report;

   open REPORT, "< $report";
   @report = <REPORT>;
   close REPORT;
   print @report;
}

sub get_Com_values
{
   # Make copy of just the Com_ values
   for(keys %stats)
   {
      if(grep /^Com_/, $_ and $stats{$_} > 0)
      {
         /^Com_(.*)/;
         $Com_vals{$1} = $stats{$_};
      }
   }

   # Remove DMS values
   delete $Com_vals{'select'};
   delete $Com_vals{'insert'};
   delete $Com_vals{'insert_select'};
   delete $Com_vals{'replace'};
   delete $Com_vals{'replace_select'};
   delete $Com_vals{'update'};
   delete $Com_vals{'update_multi'} if exists $Com_vals{'update_multi'};
   delete $Com_vals{'delete'};
   delete $Com_vals{'delete_multi'} if exists $Com_vals{'delete_multi'};
}

sub write_DTQ # Write DTQ report in descending order by values
{
   $~ = 'DTQ';

   my %DTQ;
   my $first = 1;

   get_Com_values();

   # Total Com values
   $stat_val = 0;
   for(values %Com_vals) { $stat_val += $_; }
   $DTQ{'Com_'} = $stat_val;

   $DTQ{'DMS'}      = $dms;
   $DTQ{'QC Hits'}  = $stats{'Qcache_hits'} if $stats{'Qcache_hits'} != 0;
   $DTQ{'COM_QUIT'} = int (($stats{'Connections'} - 2) - ($stats{'Aborted_clients'} / 2));

   $stat_val = 0;
   for(values %DTQ) { $stat_val += $_; }
   if($questions != $stat_val) {
      $DTQ{($questions > $stat_val ? '+Unknown' : '-Unknown')} = abs $questions - $stat_val;
   }

   for(sort { $DTQ{$b} <=> $DTQ{$a} } keys(%DTQ))
   {
      if($first) { $stat_label = '%Total:'; $first = 0; }
      else       { $stat_label = ''; }

      $stat_name = $_;
      $stat_val  = $DTQ{$_};
      write;
   }
}

sub write_DMS # Write DMS report in descending order by values
{
   $~ = 'DMS';

   for(sort { $DMS_vals{$b} <=> $DMS_vals{$a} } keys(%DMS_vals))
   {
      $stat_name = $_;
      $stat_val  = $DMS_vals{$_};
      write;
   }
}

sub write_Com # Write COM report in descending order by values
{
   $~ = 'COM_1';

   get_Com_values() if !$op{'dtq'};

   # Total Com values and write first line of COM report
   $stat_label = '%Total:' unless $op{'dtq'};
   $stat_val   = 0;
   for(values %Com_vals) { $stat_val += $_; }
   write;

   $~ = 'COM_2';

   # Sort remaining Com values, print only the top $op{'com'} number of values
   for(sort { $Com_vals{$b} <=> $Com_vals{$a} } keys(%Com_vals))
   {
      $stat_name = $_;
      $stat_val  = $Com_vals{$_};
      write;

      last if !(--$op{'com'});
   }
}

sub write_qcache
{
   # Query cache was added in 4.0.1, but have_query_cache was added in 4.0.2,
   # ergo this method is slightly more reliable
   return if not exists $vars{'query_cache_size'};
   return if $vars{'query_cache_size'} == 0;

   $qc_mem_used = $vars{'query_cache_size'} - $stats{'Qcache_free_memory'};
   $qc_hi_r = sprintf "%.2f", $stats{'Qcache_hits'} / ($stats{'Qcache_inserts'} ||= 1);
   $qc_ip_r = sprintf "%.2f", $stats{'Qcache_inserts'} / ($stats{'Qcache_lowmem_prunes'} ||= 1);

   $~ = 'QCACHE';
   write;
}

sub write_InnoDB
{
   # Innodb_ status values were added in MySQL 5.0.2
   if(! ($major >= 5 && (($minor == 0 && $patch >= 2) || $minor > 0)) )
   {
      # In case infile has Innodb_ values but didn't specify the MySQL version
      if(not defined $stats{'Innodb_page_size'}) { return; }
   }

   $~ = 'IB';
   write;

   # Innodb_row_lock_ values were added in MySQL 5.0.3
   if((($minor == 0 && $patch >= 3) || $minor > 0))
   {
      $~ = 'IB_LOCK';
      write;
   }

   if($op{'dpr'} || $op{'all'}) # Data, Pages, Rows
   {
      $~ = 'IB_DPR';
      write;
   }
}

sub have_op
{
   my $key = shift;
   return 1 if (exists $op{$key} && $op{$key} ne '');
   return 0;
}


#
# Formats
#

format MYSQL_TIME =
MySQL @<<<<<<<<<<<<<<<<  uptime @<<<<<<<<<<<   @>>>>>>>>>>>>>>>>>>>>>>>>
$vars{'version'}, sec_to_dhms($stats{'Uptime'}), scalar localtime
.

format KEY_BUFF_MAX =

__ Key _________________________________________________________________
Buffer used   @>>>>>> of @>>>>>>  %Used: @>>>>>
make_short($key_buffer_used, 1), make_short($vars{'key_buffer_size'}, 1), perc($key_buffer_used, $vars{'key_buffer_size'})
.

format KEY_BUFF_USAGE =
  Current     @>>>>>>            %Usage: @>>>>>
make_short($key_buffer_usage, 1), perc($key_buffer_usage, $vars{'key_buffer_size'})
.

format KEY_RATIOS =
Write ratio   @>>>>>>
$key_write_ratio
Read ratio    @>>>>>>
$key_read_ratio

__ Questions ___________________________________________________________
Total       @>>>>>>>>  @>>>>>/s
make_short($questions), t($questions)
.

format DTQ =
  @<<<<<<<  @>>>>>>>>  @>>>>>/s  @>>>>>> @>>>>>
$stat_name, make_short($stat_val), t($stat_val), $stat_label, perc($stat_val, $questions)
.

format SLOW_DMS =
Slow        @>>>>>>>>  @>>>>>/s  @>>>>>> @>>>>>  %DMS: @>>>>>
make_short($stats{'Slow_queries'}), t($stats{'Slow_queries'}), ($op{'dtq'} || $op{'all'} ? '' : '%Total:'), perc($stats{'Slow_queries'}, $questions), perc($stats{'Slow_queries'}, $dms)
DMS         @>>>>>>>>  @>>>>>/s          @>>>>>
make_short($dms), t($dms), perc($dms, $questions)
.

format DMS =
  @<<<<<<<  @>>>>>>>>  @>>>>>/s          @>>>>>        @>>>>>
$stat_name, make_short($stat_val), t($stat_val), perc($stat_val, $questions), perc($stat_val, $dms)
.

format COM_1 =
Com_        @>>>>>>>>  @>>>>>/s          @>>>>>
make_short($stat_val), t($stat_val), perc($stat_val, $questions)
.

format COM_2 =
  @<<<<<<<<<< @>>>>>>  @>>>>>/s          @>>>>>
$stat_name, make_short($stat_val), t($stat_val), perc($stat_val, $questions)
.

format SAS =

__ SELECT and Sort _____________________________________________________
Scan          @>>>>>>   @>>>>/s %SELECT: @>>>>>
make_short($stats{'Select_scan'}), t($stats{'Select_scan'}), perc($stats{'Select_scan'}, $stats{'Com_select'})
Range         @>>>>>>   @>>>>/s          @>>>>>
make_short($stats{'Select_range'}), t($stats{'Select_range'}), perc($stats{'Select_range'}, $stats{'Com_select'})
Full join     @>>>>>>   @>>>>/s          @>>>>>
make_short($stats{'Select_full_join'}), t($stats{'Select_full_join'}), perc($stats{'Select_full_join'}, $stats{'Com_select'})
Range check   @>>>>>>   @>>>>/s          @>>>>>
make_short($stats{'Select_range_check'}), t($stats{'Select_range_check'}), perc($stats{'Select_range_check'}, $stats{'Com_select'})
Full rng join @>>>>>>   @>>>>/s          @>>>>>
make_short($stats{'Select_full_range_join'}), t($stats{'Select_full_range_join'}), perc($stats{'Select_full_range_join'}, $stats{'Com_select'})
Sort scan     @>>>>>>   @>>>>/s
make_short($stats{'Sort_scan'}), t($stats{'Sort_scan'})
Sort range    @>>>>>>   @>>>>/s
make_short($stats{'Sort_range'}), t($stats{'Sort_range'})
Sort mrg pass @>>>>>>   @>>>>/s
make_short($stats{'Sort_merge_passes'}), t($stats{'Sort_merge_passes'})
.

format QCACHE =

__ Query Cache _________________________________________________________
Memory usage  @>>>>>> of @>>>>>>  %Used: @>>>>>
make_short($qc_mem_used, 1), make_short($vars{'query_cache_size'}, 1), perc($qc_mem_used, $vars{'query_cache_size'})
Block Fragmnt @>>>>>%
perc($stats{'Qcache_free_blocks'}, $stats{'Qcache_total_blocks'})
Hits          @>>>>>>   @>>>>/s
make_short($stats{'Qcache_hits'}), t($stats{'Qcache_hits'})
Inserts       @>>>>>>   @>>>>/s
make_short($stats{'Qcache_inserts'}), t($stats{'Qcache_inserts'})
Insrt:Prune @>>>>>>:1   @>>>>/s
make_short($qc_ip_r), t($stats{'Qcache_inserts'} - $stats{'Qcache_lowmem_prunes'})
Hit:Insert  @>>>>>>:1
$qc_hi_r, t($qc_hi_r)
.

# Not really the end...
format REPORT_END =

__ Table Locks _________________________________________________________
Waited      @>>>>>>>>  @>>>>>/s  %Total: @>>>>>
make_short($stats{'Table_locks_waited'}), t($stats{'Table_locks_waited'}), perc($stats{'Table_locks_waited'}, $stats{'Table_locks_waited'} + $stats{'Table_locks_immediate'});
Immediate   @>>>>>>>>  @>>>>>/s
make_short($stats{'Table_locks_immediate'}), t($stats{'Table_locks_immediate'})

__ Tables ______________________________________________________________
Open        @>>>>>>>> of @>>>    %Cache: @>>>>>
make_short($stats{'Open_tables'}), $vars{'table_cache'}, perc($stats{'Open_tables'}, $vars{'table_cache'})
Opened      @>>>>>>>>  @>>>>>/s
make_short($stats{'Opened_tables'}), t($stats{'Opened_tables'})

__ Connections _________________________________________________________
Max used    @>>>>>>>> of @>>>      %Max: @>>>>>
$stats{'Max_used_connections'}, $vars{'max_connections'}, perc($stats{'Max_used_connections'}, $vars{'max_connections'})
Total       @>>>>>>>>  @>>>>>/s
make_short($stats{'Connections'}), t($stats{'Connections'})

__ Created Temp ________________________________________________________
Disk table  @>>>>>>>>  @>>>>>/s
make_short($stats{'Created_tmp_disk_tables'}), t($stats{'Created_tmp_disk_tables'})
Table       @>>>>>>>>  @>>>>>/s
make_short($stats{'Created_tmp_tables'}), t($stats{'Created_tmp_tables'})
File        @>>>>>>>>  @>>>>>/s
make_short($stats{'Created_tmp_files'}), t($stats{'Created_tmp_files'})
.

format TAB =

__ Threads _____________________________________________________________
Running     @>>>>>>>> of @>>>
$stats{'Threads_running'}, $stats{'Threads_connected'}
Cached      @>>>>>>>> of @>>>      %Hit: @>>>>>
$stats{'Threads_cached'}, $vars{'thread_cache_size'}, make_short(100 - perc($stats{'Threads_created'}, $stats{'Connections'}))
Created     @>>>>>>>>  @>>>>>/s
make_short($stats{'Threads_created'}), t($stats{'Threads_created'})
Slow        @>>>>>>>>  @>>>>>/s
$stats{'Slow_launch_threads'}, t($stats{'Slow_launch_threads'})

__ Aborted _____________________________________________________________
Clients     @>>>>>>>>  @>>>>>/s
make_short($stats{'Aborted_clients'}), t($stats{'Aborted_clients'})
Connects    @>>>>>>>>  @>>>>>/s
make_short($stats{'Aborted_connects'}), t($stats{'Aborted_connects'})

__ Bytes _______________________________________________________________
Sent        @>>>>>>>>  @>>>>>/s
make_short($stats{'Bytes_sent'}), t($stats{'Bytes_sent'})
Received    @>>>>>>>>  @>>>>>/s
make_short($stats{'Bytes_received'}), t($stats{'Bytes_received'})
.

format IB =

__ InnoDB Buffer Pool __________________________________________________
Usage         @>>>>>> of @>>>>>>  %Used: @>>>>>
make_short($ib_bp_used, 1), make_short($ib_bp_total, 1), perc($ib_bp_used, $ib_bp_total)
Read ratio    @>>>>>>
$ib_bp_read_ratio;
Pages
  Free      @>>>>>>>>            %Total: @>>>>>
make_short($stats{'Innodb_buffer_pool_pages_free'}), perc($stats{'Innodb_buffer_pool_pages_free'}, $stats{'Innodb_buffer_pool_pages_total'})
  Data      @>>>>>>>>                    @>>>>> %Drty: @>>>>>
make_short($stats{'Innodb_buffer_pool_pages_data'}), perc($stats{'Innodb_buffer_pool_pages_data'}, $stats{'Innodb_buffer_pool_pages_total'}), perc($stats{'Innodb_buffer_pool_pages_dirty'}, $stats{'Innodb_buffer_pool_pages_data'})
  Misc      @>>>>>>>>                    @>>>>>
  $stats{'Innodb_buffer_pool_pages_misc'}, perc($stats{'Innodb_buffer_pool_pages_misc'}, $stats{'Innodb_buffer_pool_pages_total'})
  Latched   @>>>>>>>>                    @>>>>>
$stats{'Innodb_buffer_pool_pages_latched'}, perc($stats{'Innodb_buffer_pool_pages_latched'}, $stats{'Innodb_buffer_pool_pages_total'})
Reads       @>>>>>>>>  @>>>>>/s  
make_short($stats{'Innodb_buffer_pool_read_requests'}), t($stats{'Innodb_buffer_pool_read_requests'})
  From file @>>>>>>>>  @>>>>>/s          @>>>>>
make_short($stats{'Innodb_buffer_pool_reads'}), t($stats{'Innodb_buffer_pool_reads'}), perc($stats{'Innodb_buffer_pool_reads'}, $stats{'Innodb_buffer_pool_read_requests'})
  Ahead Rnd @>>>>>>>>  @>>>>>/s
$stats{'Innodb_buffer_pool_read_ahead_rnd'}, t($stats{'Innodb_buffer_pool_read_ahead_rnd'})
  Ahead Sql @>>>>>>>>  @>>>>>/s
$stats{'Innodb_buffer_pool_read_ahead_seq'}, t($stats{'Innodb_buffer_pool_read_ahead_seq'})
Writes      @>>>>>>>>  @>>>>>/s
make_short($stats{'Innodb_buffer_pool_write_requests'}), t($stats{'Innodb_buffer_pool_write_requests'})
Flushes     @>>>>>>>>  @>>>>>/s
make_short($stats{'Innodb_buffer_pool_pages_flushed'}), t($stats{'Innodb_buffer_pool_pages_flushed'})
Wait Free   @>>>>>>>>  @>>>>>/s
$stats{'Innodb_buffer_pool_wait_free'}, t($stats{'Innodb_buffer_pool_wait_free'})
.

format IB_LOCK =

__ InnoDB Lock _________________________________________________________
Waits       @>>>>>>>>  @>>>>>/s
$stats{'Innodb_row_lock_waits'}, t($stats{'Innodb_row_lock_waits'})
Current     @>>>>>>>>
$stats{'Innodb_row_lock_current_waits'}
Time acquiring
  Total     @>>>>>>>> ms
$stats{'Innodb_row_lock_time'}
  Average   @>>>>>>>> ms
$stats{'Innodb_row_lock_time_avg'}
  Max       @>>>>>>>> ms
$stats{'Innodb_row_lock_time_max'}
.

format IB_DPR =

__ InnoDB Data, Pages, Rows ____________________________________________
Data
  Reads     @>>>>>>>>  @>>>>>/s
make_short($stats{'Innodb_data_reads'}), t($stats{'Innodb_data_reads'})
  Writes    @>>>>>>>>  @>>>>>/s
make_short($stats{'Innodb_data_writes'}), t($stats{'Innodb_data_writes'})
  fsync     @>>>>>>>>  @>>>>>/s
make_short($stats{'Innodb_data_fsyncs'}), t($stats{'Innodb_data_fsyncs'})
  Pending
    Reads   @>>>>>>>>
$stats{'Innodb_data_pending_reads'}, t($stats{'Innodb_data_pending_reads'})
    Writes  @>>>>>>>>
$stats{'Innodb_data_pending_writes'}, t($stats{'Innodb_data_pending_writes'})
    fysnc   @>>>>>>>>
$stats{'Innodb_data_pending_fsyncs'}, t($stats{'Innodb_data_pending_fysncs'})

Pages
  Created   @>>>>>>>>  @>>>>>/s
make_short($stats{'Innodb_pages_created'}), t($stats{'Innodb_pages_created'})
  Read      @>>>>>>>>  @>>>>>/s
make_short($stats{'Innodb_pages_read'}), t($stats{'Innodb_pages_read'})
  Written   @>>>>>>>>  @>>>>>/s
make_short($stats{'Innodb_pages_written'}), t($stats{'Innodb_pages_written'})

Rows
  Deleted   @>>>>>>>>  @>>>>>/s
make_short($stats{'Innodb_rows_deleted'}), t($stats{'Innodb_rows_deleted'})
  Inserted  @>>>>>>>>  @>>>>>/s
make_short($stats{'Innodb_rows_inserted'}), t($stats{'Innodb_rows_inserted'})
  Read      @>>>>>>>>  @>>>>>/s
make_short($stats{'Innodb_rows_read'}), t($stats{'Innodb_rows_read'})
  Updated   @>>>>>>>>  @>>>>>/s
make_short($stats{'Innodb_rows_updated'}), t($stats{'Innodb_rows_updated'})
.

