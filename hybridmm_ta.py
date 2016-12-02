# -*- coding: utf-8 -*-
#import os.path
#import re
#filename = os.path.normpath('D:/lab/pro_page cache_hybrid raid/trace analysis/nd_filebench5')
import codecs
import time
import traceback
import sys
filename = '../data/fb_networkfs_10m.log'
HEADLINENUMBER = 11
MINLENGH = 10
READ_FUNCTION_NAME = "generic_file_read_iter"


page = {}    
writereq = 0
readreq = 0
overwritereq = 0
# unique_page_number = 0
MAX_OVERWRITE_NUM = 10
state = [0] * MAX_OVERWRITE_NUM
DIRTY_THROT = 30
FLUSH_MODE = 0
TIME_WINDOW = 60
current = 0
f = open("../data/result.log", "a")
'''
main overflow of the program
'''
def load_file(filename):
    i = 0
    line = ''
    timer = 0
    r = 0
    indexNum = 0
    tempReq = 0
    global writereq,state,page,unique_page_number,current,readreq,f
    print(page)
    try:
        
        # with codecs.open(filename, 'r', encoding='utf-8', errors='ignore') as fin:
        #     for line in fin:

        s = time.time()
        fin = codecs.open(filename, 'r', encoding='utf-8', errors='ignore')          
        lines = fin.readlines()
        e = time.time()
        print("load file consumed ", (e-s), "s")
        s = e
        lineNum = len(lines)
        for line in lines:
            i=i+1
            # print(i, readreq, writereq)
#            if line is null
            if len(line)<=10:
                continue
#            get head information
            if i==10:
                task_tail_loc = line.find('-')
                print(task_tail_loc)
                cpu_loc = line.find('CPU')
                print(cpu_loc)
                timestamp_loc = cpu_loc + 10
                function_loc = line.find('FUNCTION')
            if i<=HEADLINENUMBER:
                continue           
#           parse the head
            task = line[0:task_tail_loc].strip()
            pid = int(line[task_tail_loc+1:cpu_loc])
            timestamp = float(line[timestamp_loc:function_loc].replace(':',''))
            # print(task, pid, timestamp)
            line = line[function_loc:]
            colon_loc = line.find(':')
            function_name = line[:colon_loc]
            # print(function_name)
            line = line[colon_loc+1:]
            items = line.split(',')
#           for return sentence
            if len(items)==2:
                continue
            j = 0
            inode = int(items[j])
            j+=1
            if function_name.strip() == READ_FUNCTION_NAME:
                # isize = int(items[j])
                # j+=1
                r = 0
                j += 1
            else:
                r = 1
            pos = int(items[j])
            j+=1
            count = int(items[j])
            j+=2
            filename = items[j]            
            current = timestamp

#           finish parse
            # if function_name!=READ_FUNCTION_NAME:
            #     print(i, task, pid, timestamp, function_name, inode, isize, pos, count, filename)
            #     break
            if not inode in page.keys():
                page[inode] = {}
            index_0 = pos >> 12
            index_1 = (pos + count - 1) >> 12
            if r == 0:
                readreq += index_1 - index_0 + 1
                continue
            for index in range(index_0, index_1 + 1):         
                writereq += 1
                if not index in page[inode].keys():
                    indexNum += 1
                    page[inode][index] = (timestamp, 0)
                else:
                    last, overwrite = page[inode][index]
                    if timestamp - last <= DIRTY_THROT:
                        page[inode][index] = (timestamp, overwrite + 1)
                    else:
                        # f.write("normal addover %d, %d, %d\n" % (inode, index, overwrite))
                        addoverwrite(overwrite)
                        page[inode][index] = (timestamp, 0)
            # if i >= 20:
            #     print_state()
            #     sys.exit(0)
            if (FLUSH_MODE==1) and (current - timer >= TIME_WINDOW):
                flush_page()
                timer = current
            if i%10000==0:
                e = time.time()
                print(i, int(100*i/lineNum), "%", indexNum, (writereq-tempReq), "consumed", (e-s), "s")
                tempReq = writereq
                s = e
    except Exception as e:
        print(i, line)
        print(len(line))
#        print ('str(Exception):\t', str(Exception))
#        print ('str(e):\t\t', str(e))
        print (repr(e))
#        print ('e.message:\t', e.message)
        print (traceback.print_exc())
        print (traceback.format_exc())
    finally:
        fin.close()
        

def flush_page():
    print(current)
    # print(page)
    global page,f
    inodes = list(page.keys())
    for inode in inodes:
        indexes = list(page[inode].keys())
        for index in indexes:
            time, overwrite = page[inode][index]
            if current - time > DIRTY_THROT:
                # f.write("flush %d, %d, %d\n" % (inode, index, overwrite))
                addoverwrite(overwrite)
                del(page[inode][index])    
        indexes = list(page[inode].keys())
        if len(indexes) == 0:
            del(page[inode])
    # print(page)
'''
record the overwrite information, including :
global overwrite number
the number of pages with the specific overwrite number
'''
def addoverwrite(overwrite):
    global state,overwritereq
    overwritereq = overwritereq + overwrite
    if overwrite>=MAX_OVERWRITE_NUM:
        overwrite=MAX_OVERWRITE_NUM-1
    state[overwrite] = state[overwrite]+1

def print_state():
    # print("Total write requst number = %d" % writereq)    
    # print('Write ratio = %f%%' % (100.0*writereq/(readreq+writereq)))
    # print("Total overwrite requst number = %d" % overwritereq)
    # print("Overwrite ratio = %f%%" % (100.0*overwritereq/writereq))
    # print(state)
    print("Total write requst number = %d" % writereq, file=f)    
    print('Write ratio = %f%%' % (100.0*writereq/(readreq+writereq)), file=f)
    print("Total overwrite requst number = %d" % overwritereq, file=f)
    print("Overwrite ratio = %f%%" % (100.0*overwritereq/writereq), file=f)
    print(state, file=f)
    print("****************************************", file=f)

def print_title(filename):
    print(filename, file=f)

'''
record all states of pages in page dict
called after the main overflow to record the pages that have not been flushed
'''
def clear_page():
    for inode in page.keys():
        inode_tree = page[inode]
        for index in inode_tree.keys():
            time, overwrite = page[inode][index]
            # f.write("clear %d, %d, %d\n" % (inode, index, overwrite))
            addoverwrite(overwrite)


def clear_state():
    global page,writereq,readreq,overwritereq,state,current
    page = {}
    writereq = 0
    readreq = 0
    overwritereq = 0
    current = 0
    state = [0] * MAX_OVERWRITE_NUM

s = time.time()
workload_list = ["../data/tpcc_test1.log"]

dt_list = [10, 60, 120]





for workload in workload_list:
    for dt in dt_list:
        DIRTY_THROT = dt
        print("dirty throttling = %d, flush mode = %d, time window = %d" % (DIRTY_THROT, FLUSH_MODE, TIME_WINDOW), file = f)
        filename = workload
        clear_state()
        print_title(filename)
        load_file(filename)
        clear_page()
        print_state()
        e = time.time()
        print('%f s' % (e-s))
        s = e
f.close()