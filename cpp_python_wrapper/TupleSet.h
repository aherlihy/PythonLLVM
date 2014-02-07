#ifndef TUPLE_SET_H
#define TUPLE_SET_H

#include <dirent.h>
#include <fstream>
#include <string>
#include <sys/stat.h>
#include <vector>

#include "compiler/ClangCompiler.h"
#include "data/Schema.h"
#include "job/JobValidator.h"
#include "net/Connection.h"
#include "net/TcpConnection.h"
#include "proto/AttributeInfo.pb.h"
#include "proto/ExpressionInfo.pb.h"
#include "proto/JobInfo.pb.h"
#include "util/Logger.h"
#include "util/LogLevel.h"
#include <boost/python.hpp>

using std::string;

namespace tupleware {
class TupleSet {
public:
    TupleSet(Input *in) {
    }

    void combine(string funct, string keyFunct) {
        ExpressionInfo *expr = new ExpressionInfo();
        expr->set_type(ExpressionInfo::COMBINE);
        CombineInfo *ext = expr->MutableExtension(CombineInfo::ext);
        ExpressionInfo *extsrc = ext->mutable_source();
        extsrc->CopyFrom(*m_expr);
        ext->set_function(funct);
        ext->set_key_function(keyFunct);
        m_expr = expr;
    }
/*
    void filter(string funct) {
        OperatorInfo *op = new OperatorInfo();
        op->set_type(OperatorInfo::FILTER);
        FilterInfo *ext = op->MutableExtension(FilterInfo::ext);
        OperatorInfo *extsrc = ext->mutable_source();
        extsrc->CopyFrom(*m_op);
        ext->set_function(funct);
        m_op = op;
    }

    void flatMap(string funct) {
        OperatorInfo *op = new OperatorInfo();
        op->set_type(OperatorInfo::FLAT_MAP);
        FlatMapInfo *ext = op->MutableExtension(FlatMapInfo::ext);
        OperatorInfo *extsrc = ext->mutable_source();
        extsrc->CopyFrom(*m_op);
        ext->set_function(funct);
        m_op = op;
    }

    void group(string funct, string keyFunct) {
        OperatorInfo *op = new OperatorInfo();
        op->set_type(OperatorInfo::GROUP);
        GroupInfo *ext = op->MutableExtension(GroupInfo::ext);
        OperatorInfo *extsrc = ext->mutable_source();
        extsrc->CopyFrom(*m_op);
        ext->set_function(funct);
        ext->set_key_function(keyFunct);
        m_op = op;
    }

    void join(TupleSet *other, string funct) {
        OperatorInfo *op = new OperatorInfo();
        op->set_type(OperatorInfo::JOIN);
        JoinInfo *ext = op->MutableExtension(JoinInfo::ext);
        OperatorInfo *extlhs = ext->mutable_left_hand_side();
        extlhs->CopyFrom(*m_op);
        OperatorInfo *extrhs = ext->mutable_right_hand_side();
        extrhs->CopyFrom(*other->m_op);
        ext->set_function(funct);
        m_op = op;
    }
*/
    void map(string funct) {
        ExpressionInfo *expr = new ExpressionInfo();
        expr->set_type(ExpressionInfo::MAP);
        MapInfo *ext = expr->MutableExtension(MapInfo::ext);
        ExpressionInfo *extsrc = ext->mutable_source();
        extsrc->CopyFrom(*m_expr);
        ext->set_function(funct);
        m_expr = expr;
    }
/*
    void merge(TupleSet *other) {
        OperatorInfo *op = new OperatorInfo();
        op->set_type(OperatorInfo::MERGE);
        MergeInfo *ext = op->MutableExtension(MergeInfo::ext);
        OperatorInfo *extlhs = ext->mutable_left_hand_side();
        extlhs->CopyFrom(*m_op);
        OperatorInfo *extrhs = ext->mutable_right_hand_side();
        extrhs->CopyFrom(*other->m_op);
        m_op = op;
    }

    void peek(int limit) {
        OperatorInfo *op = new OperatorInfo();
        op->set_type(OperatorInfo::PEEK);
        PeekInfo *ext = op->MutableExtension(PeekInfo::ext);
        OperatorInfo *extsrc = ext->mutable_source();
        extsrc->CopyFrom(*m_op);
        ext->set_limit(limit);
        m_op = op;
    }
*/
    void reduce(string funct, string keyFunct) {
        ExpressionInfo *expr = new ExpressionInfo();
        expr->set_type(ExpressionInfo::REDUCE);
        ReduceInfo *ext = expr->MutableExtension(ReduceInfo::ext);
        ExpressionInfo *extsrc = ext->mutable_source();
        extsrc->CopyFrom(*m_expr);
        ext->set_function(funct);
        ext->set_key_function(keyFunct);
        //ext->set_is_parallel(isParallel);
        m_expr = expr;
    }

    void evaluate(string addr, string functDir) {
        int jport = TcpConnection::JOB_PORT;
        TcpConnection *jconn = TcpConnection::push(addr, jport);
        jconn->open(CONNECT);

        //job
        JobInfo *job = new JobInfo();
        job->set_client("test");
        job->set_allocated_expression(m_expr);

        //UDFs
        DIR *dir;
        if (functDir.length() > 0
                || (dir = opendir(functDir.c_str())) == NULL) {
            Logger::log(FATAL, __FILE__, __LINE__, "bad function directory");
            return;
        }
        else {
            struct dirent *file;
            struct stat filestat;
            while ((file = readdir(dir))) {
                string filename = functDir + "/" + file->d_name;
                if (stat(filename.c_str(), &filestat)
                        || S_ISDIR(filestat.st_mode))
                    continue;

                string functName = file->d_name;
                functName = functName.substr(0, functName.find("."));
                FunctionInfo *funct = job->add_function();
                funct->set_name(functName);
                funct->set_llvm(ClangCompiler::compile(filename));

                ifstream rdr;
                rdr.open(filename);
                if (rdr.is_open()) {
                    string line;
                    while (!rdr.eof()) {
                        int pos = 0;
                        getline(rdr, line);
                        pos = line.find("@", pos);
                        if (pos != string::npos) {
                            string type = line.substr(pos + 1);
                            AttributeInfo *attr = funct->add_attribute();
                            if (type.compare("double") == 0)
                                attr->set_type(AttributeInfo::DOUBLE);
                            else if (type.compare("float") == 0)
                                attr->set_type(AttributeInfo::FLOAT);
                            else if (type.compare("int") == 0)
                                attr->set_type(AttributeInfo::INT);
                            else if (type.compare("string") == 0) {
                                attr->set_type(AttributeInfo::STRING);
                                attr->set_length(30);
                            }
                        }
                    }
                    rdr.close();
                }
            }
            closedir(dir);

            if (!JobValidator::isValid(job))
                return;

            jconn->send(job);
        }
        jconn->close();
        delete job;

        int rport = TcpConnection::RESULT_PORT;
        TcpConnection *rconn = NULL;
    }
protected:
    ExpressionInfo *m_expr;

    void addAttribute(Schema *sch, int attrNum, AttributeInfo *attr) {
        switch (sch->getType(attrNum)) {
            case DOUBLE:
                attr->set_type(AttributeInfo::DOUBLE);
                break;
            case FLOAT:
                attr->set_type(AttributeInfo::FLOAT);
                break;
            case INT:
                attr->set_type(AttributeInfo::INT);
                break;
            case STRING:
                attr->set_type(AttributeInfo::STRING);
                attr->set_length(sch->getLength(attrNum));
                break;
            default:
                break;
        }
    }
};

// expose classes/functions to python
BOOST_PYTHON_MODULE(python_example)
{
    class_<Input>("Input");
    class_<TupleSet>("TupleSet", init<Input*>())
        .def("combine", &TupleSet::combine)
        .def("map", &TupleSet::map)
        .def("reduce", &TupleSet::reduce)
        .def("evaluate", &TupleSet::evaluate)
    ;
}
#endif
