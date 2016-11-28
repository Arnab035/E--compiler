#include "Ast.h"					
#include "ParserUtil.h"					


AstNode::AstNode(NodeType nt, int line, int column, string file):
  ProgramElem(NULL, line, column, file) {
	// Add your code here
	nodeType_ = nt;
};

AstNode::AstNode(const AstNode& ast): ProgramElem(ast) {
	// Add your code here
	nodeType_ = ast.nodeType_;
};

/****************************************************************/

ExprNode::ExprNode(ExprNodeType et, const Value* val, int line, int column, 
				   string file):
	AstNode(AstNode::NodeType::EXPR_NODE, line, column, file)
{
	// Add your code here
	exprType_ = et;
	val_ = val;
	
};

ExprNode::ExprNode(const ExprNode& e) : AstNode(e)
{
	// Add your code here
	exprType_ = e.exprType_ ;
	val_ = e.val_;
};

void ExprNode::print(ostream& out, int indent) const {
	
	if((this)->exprNodeType()==ExprNode::ExprNodeType::REF_EXPR_NODE){
				const RefExprNode* refexp;
				refexp = dynamic_cast<const RefExprNode* >(this);
				if(refexp != nullptr)
					refexp->print(out, indent);
			}

	else if((this)->exprNodeType()==ExprNode::ExprNodeType::VALUE_NODE){
				const ValueNode* val;
				val = dynamic_cast<const ValueNode* >(this);
				if(val != nullptr){
					val->print(out, indent);
				}
					
			}

	else if((this)->exprNodeType()==ExprNode::ExprNodeType::OP_NODE){

				const OpNode* op;
				op = dynamic_cast<const OpNode* >(this);
				if(op != nullptr){
					op->print(out, indent);
				}
	}
	else if((this)->exprNodeType()==ExprNode::ExprNodeType::INV_NODE){

				const InvocationNode* inv;
				inv = dynamic_cast<const InvocationNode* >(this);
				if(inv != nullptr){
					inv->print(out, indent);
				}
	}
};


void ExprNode::typePrint(ostream& out, int indent) const {
	
	if((this)->exprNodeType()==ExprNode::ExprNodeType::REF_EXPR_NODE){
				const RefExprNode* refexp;
				refexp = dynamic_cast<const RefExprNode* >(this);
				if(refexp != nullptr)
					refexp->typePrint(out, indent);
			}

	else if((this)->exprNodeType()==ExprNode::ExprNodeType::VALUE_NODE){
				const ValueNode* val;
				val = dynamic_cast<const ValueNode* >(this);
				if(val != nullptr){
					val->typePrint(out, indent);
				}
					
			}

	else if((this)->exprNodeType()==ExprNode::ExprNodeType::OP_NODE){

				const OpNode* op;
				op = dynamic_cast<const OpNode* >(this);
				if(op != nullptr){
					op->typePrint(out, indent);
				}
	}
	else if((this)->exprNodeType()==ExprNode::ExprNodeType::INV_NODE){

				const InvocationNode* inv;
				inv = dynamic_cast<const InvocationNode* >(this);
				if(inv != nullptr){
					inv->typePrint(out, indent);
				}
	}
	
}

const Type* ExprNode::typeCheck() const{
	const Type* exprType = 0;
	if((this)->exprNodeType()==ExprNode::ExprNodeType::REF_EXPR_NODE){
		const RefExprNode* refexp;
		refexp = dynamic_cast<const RefExprNode* >(this);

		if(refexp != nullptr){
			exprType = refexp->typeCheck();
		}
	}
	else if((this)->exprNodeType()==ExprNode::ExprNodeType::VALUE_NODE){
		const ValueNode* val;
		val = dynamic_cast<const ValueNode* >(this);

		if(val != nullptr){
			exprType = val->typeCheck();
		}
	}
	else if((this)->exprNodeType()==ExprNode::ExprNodeType::OP_NODE){
		const OpNode* op;
		op = dynamic_cast<const OpNode* >(this);

		if(op != nullptr){
			exprType = op->typeCheck();
		}			
		
	}
	else if((this)->exprNodeType()==ExprNode::ExprNodeType::INV_NODE){
		const InvocationNode* inv;

		inv = dynamic_cast<const InvocationNode* >(this);

		if(inv != nullptr){
			exprType =inv->typeCheck();
		}
	}
	if(exprType == NULL){
		const Type* nullType = new Type(Type::TypeTag::UNKNOWN);
		return nullType;
	}
	return exprType;
}



RuleNode::RuleNode(BlockEntry *re, BasePatNode* pat, StmtNode* reaction, 
	   int line, int column, string file):
	AstNode(AstNode::NodeType::RULE_NODE, line, column, file){
	// Add your code here
	rste_ = re;
	pat_ = pat;
	reaction_ = reaction;
}

void RuleNode::print(ostream& out, int indent) const{

	// Code to print rulenode
	const BasePatNode* bpn = pat();
	if(bpn->kind()==BasePatNode::PatNodeKind::PRIMITIVE){
		const PrimitivePatNode* ppn = (const PrimitivePatNode*)(bpn);
		ppn->print(out, indent);
	}
	else{	
		const PatNode* pan = (const PatNode* )(bpn);
		pan->print(out, indent);
	}
	out << " -> ";
	const StmtNode* stm = reaction();
	stm->print(out, indent);
	out << endl;	 
}

void RuleNode::typePrint(ostream& out, int indent) const {

        // Code to type print rulenode
	const BasePatNode* bpn = pat();
	if(bpn->kind()==BasePatNode::PatNodeKind::PRIMITIVE){
		const PrimitivePatNode* ppn = (const PrimitivePatNode*)(bpn);
		ppn->typePrint(out, indent);
	}
	else{	
		const PatNode* pan = (const PatNode* )(bpn);
		pan->typePrint(out, indent);
	}
	out << " -> ";
	const StmtNode* stm = reaction();
	stm->typePrint(out, indent);
	out << endl;	 

}

// add type checks for rules also

InvocationNode::InvocationNode(const SymTabEntry *ste, vector<ExprNode*>* param, 
		 int line, int column, string file):
                ExprNode(ExprNode::ExprNodeType::INV_NODE, 0, line, column, file){
	// Code to initialize invocationnode privates

	ste_ = ste;
	params_ = param;

}

// compound statement type checking

void CompoundStmtNode::print(ostream& out, int indent) const {

	// code to print compound statement node
	out << "{ ";
	const list<StmtNode*>* liststmt = stmts();
	StmtNode* stm;
	for (list<StmtNode* >::const_iterator i = liststmt->begin(); i!= liststmt->end(); ++i){
		stm = *i;
		
		stm->print(out, indent);  
		out << ";";
		out << endl;
	} 
	out << endl << "}";
}

void CompoundStmtNode::typePrint(ostream& out, int indent) const {

	// code to print type of compound statement node
	out << "{ ";
	const list<StmtNode*>* liststmt = stmts();
	StmtNode* stm;
	for (list<StmtNode* >::const_iterator i = liststmt->begin(); i!= liststmt->end(); ++i){
		stm = *i;
		
		stm->typePrint(out, indent);  
		out << ";";
		out << endl;
	} 
	out << endl << "}";
}

const Type* CompoundStmtNode::typeCheck() const {

	// code to check type of Compound statement
	const list<StmtNode*>* liststmt = stmts();
	StmtNode* stm;
	for (list<StmtNode* >::const_iterator i = liststmt->begin(); i!= liststmt->end(); ++i){
		stm = *i;
		stm->typeCheck();  
	} 
	return NULL;
}

void CompoundStmtNode::printWithoutBraces(ostream& out, int indent) const {
	// print without braces
	const list<StmtNode*>* liststmt = stmts();
	StmtNode* stm;
	for (list<StmtNode* >::const_iterator i = liststmt->begin(); i!= liststmt->end(); ++i){
		stm = *i;
		stm->print(out, indent);
		out << ";";
		out << endl;
	} 
}

InvocationNode::InvocationNode(const InvocationNode& inv): ExprNode(inv){

	// Copy constructor	
	ste_ = inv.ste_ ;
	params_ = inv.params_ ;

}

void InvocationNode::print(ostream& out, int indent) const{

	// Code to print invocation node
	out << ste_->name();
	out << " (" ;
	const vector<ExprNode*>* pars = params();
	if(pars != nullptr){
		ExprNode* exp;
		for (vector<ExprNode* >::const_iterator i = pars->begin(); i != pars->end(); ++i) {
		        exp = *i;
			exp->print(out, indent) ;
			out << "," ;
		}
	}
	out << " )";
	out << endl;
	
}

void InvocationNode::typePrint(ostream& out, int indent) const{

	// Code to print invocation node
	out << ste_->name();
	out << " (" ;
	const vector<ExprNode*>* pars = params();
	if(pars != nullptr){
		ExprNode* exp;
		for (vector<ExprNode* >::const_iterator i = pars->begin(); i != pars->end(); ++i) {
		        exp = *i;
			exp->typePrint(out, indent) ;
			out << "," ;
		}
	}
	out << " )";
	out << endl;
	
}

// type checking for invocation node

const Type* InvocationNode::typeCheck() const{
	unsigned int j;
	const Type* t1;
	
	
	const SymTab* map_;
	// gives warning later 
	const SymTabEntry* ste = ste_;
	
	// Get the params
	const vector<ExprNode*>* pars = params();
	const ExprNode* exp;
	if(pars != nullptr){
		
		map_ = ste->symTab();
		SymTab::const_iterator i = map_->begin();

		for(j=0; j< pars->size(); j++,++i){
			exp = (*pars)[j];
			t1 = exp->typeCheck();
			// check if this type is a subtype of original formal args
			VariableEntry* v = (VariableEntry* )(*i);

			Type* fparamType = v->type();
			
			
			if((Type::isNumeric(fparamType->tag()) && !(Type::isNumeric(t1->tag()))) || 
			    (!(Type::isNumeric(fparamType->tag())) && Type::isNumeric(t1->tag()))){
				
					errMsg("Formal parameter and actual do not match");
				
			}
			if((Type::isBool(fparamType->tag()) && !(Type::isBool(t1->tag()))) || 
			    (!(Type::isBool(fparamType->tag())) && Type::isBool(t1->tag())))
				{
					errMsg("Formal parameter and actual do not match");
				}
			
			if((fparamType->tag()==Type::TypeTag::CLASS && t1->tag() != Type::TypeTag::CLASS) ||
			    (fparamType->tag() != Type::TypeTag::CLASS && t1->tag() == Type::TypeTag::CLASS))
				{
				
					errMsg("Formal parameter and actual do not match");
				}
			}
			
		}
	
	if(ste == NULL){
		const Type* nullType = new Type(Type::TypeTag::UNKNOWN);
		return nullType;
	}
	return ste->type(); // return the return type of the function
}



RefExprNode::RefExprNode(string ext, const SymTabEntry* ste, 
	      int line, int column, string file):
	     ExprNode(ExprNode::ExprNodeType::REF_EXPR_NODE, 0, line, column, file){
	
	ext_ = ext;
	sym_ = ste;
}

RefExprNode::RefExprNode(const RefExprNode& ref): ExprNode(ref){
	
	ext_ = ref.ext_;
	sym_ = ref.sym_;

}

void RefExprNode::print(ostream& out, int indent) const{

	// Code to print refexpr node
	string lval = ext();
	out << lval;
	out << " ";
}

void RefExprNode::typePrint(ostream& out, int indent) const{

	// Code to print types of refexpr node
	const SymTabEntry* ste = symTabEntry();
        const Type* typ = ste->type();
	typ->print(out, indent);

}


IfNode::IfNode(ExprNode* cond, StmtNode* thenStmt, 
		 StmtNode* elseStmt, int line, int column, string file):
	StmtNode(StmtNode::StmtNodeKind::IF, line, column, file){
	cond_ = cond;
	then_ = thenStmt;
	if(elseStmt != NULL){
		else_ = elseStmt;
	}    
}


void IfNode::print(ostream& out, int indent) const {

// code to print IfNode
	const ExprNode* c = cond();
	const StmtNode* th = thenStmt();
	const StmtNode* el = elseStmt();
	out << "if(  ";
	c->print(out, indent);
	out << " )";
	out << endl;
	th->print(out, indent);
	if( el != NULL){
		out << endl;
		el-> print(out, indent);
	} 
}

void IfNode::typePrint(ostream& out, int indent)const {

// code to print type of IfNode
	const ExprNode* c = cond();
	const StmtNode* th = thenStmt();
	const StmtNode* el = elseStmt();
	out << "if(  ";
	c->typePrint(out, indent);
	out << " )";
	out << endl;
	th->typePrint(out, indent);
	if( el != NULL){
		out << endl;
		el-> typePrint(out, indent);
	} 

}

const Type* IfNode::typeCheck() const{
	const ExprNode* c = cond();
	
	const Type* t1 = c->typeCheck();
	if(!Type::isBool(t1->tag())){
		errMsg("If conditions need a Boolean statement");
	}

	return NULL;
};



PatNode::PatNode(PatNodeKind pk, BasePatNode *p1, BasePatNode*p2, int line, int column, string file):
	BasePatNode(pk, line, column, file){
	
	pat1_ = p1;
	if(p2 != NULL){
		pat2_ = p2;
	}
}

void PatNode::print(ostream& out, int indent) const {

// code to print patnode.

	const BasePatNode* bp1 = pat1();
	const BasePatNode* bp2 = pat2();
	
	PatNodeKind pnk = kind();
	if(pnk == BasePatNode::PatNodeKind::NEG){
		out << "!";
		bp1->print(out,indent);
	
	}
	else if(pnk == BasePatNode::PatNodeKind::OR){
		bp1->print(out, indent);
		out << "\\/";
		bp2->print(out, indent);
		
	}
	else if(pnk == BasePatNode::PatNodeKind::STAR){
		out << "**" ;
		bp1-> print(out, indent);
		
	}
	else if(pnk == BasePatNode::PatNodeKind::SEQ){
		bp1->print(out, indent);			
		out << ":";
		bp2->print(out, indent);
		
	}
}

void PatNode::typePrint(ostream& out, int indent) const {

// code to print patnode.

	const BasePatNode* bp1 = pat1();
	const BasePatNode* bp2 = pat2();
	
	PatNodeKind pnk = kind();
	if(pnk == BasePatNode::PatNodeKind::NEG){
		out << "!";
		bp1->typePrint(out,indent);
	
	}
	else if(pnk == BasePatNode::PatNodeKind::OR){
		bp1->typePrint(out, indent);
		out << "\\/";
		bp2->typePrint(out, indent);
		
	}
	else if(pnk == BasePatNode::PatNodeKind::STAR){
		out << "**" ;
		bp1-> typePrint(out, indent);
		
	}
	else if(pnk == BasePatNode::PatNodeKind::SEQ){
		bp1->typePrint(out, indent);			
		out << ":";
		bp2->typePrint(out, indent);
		
	}
}


const Type* PatNode::typeCheck() const {


	return NULL;
	

}


bool PatNode::hasNeg() const{

	return false;
}

bool PatNode::hasSeqOps() const{

	return false;

}

bool PatNode::hasAnyOrOther() const{

	return false;

}

PrimitivePatNode::PrimitivePatNode(EventEntry* ee, vector<VariableEntry*>* params, 
				   ExprNode* c,
				   int line, int column, string file):
	BasePatNode(BasePatNode::PatNodeKind::PRIMITIVE, line, column, file){
	ee_ = ee;
	params_ = params;
	if(c != NULL){
		cond_ = c;
	}
	// only assignment expressions do not go to condition_
	if(c != NULL){
		if(c->exprNodeType() != ExprNode::ExprNodeType::REF_EXPR_NODE){
			condition_ = c;	
		}
	}
}

bool PrimitivePatNode::hasNeg() const{

	return false;
}

bool PrimitivePatNode::hasSeqOps() const{

	return false;

}

bool PrimitivePatNode::hasAnyOrOther() const{

	return false;

}

void PrimitivePatNode::print(ostream &out, int indent) const {
// code to print patnode.
	const EventEntry* eve = event();
	out << "(";
	if(eve != NULL){
		out << eve->name() ;
		out << " (" ;
		const vector<const VariableEntry* >* pars = params();
		const VariableEntry* var;
		for (vector<const VariableEntry* >::const_iterator i = pars->begin(); i != pars->end(); ++i) {
			var = *i;
			out << var->name()  ;
			out << "," ;
		} 
		out << " )" ;
	}
	if (cond_ != NULL) {
		out << "| ";
		const ExprNode* c = cond();
		c -> print(out, indent); // what indent to take ?
	}
	out << ")";
	
}

void PrimitivePatNode::typePrint(ostream &out, int indent) const {
// code to print patnode.
	const EventEntry* eve = event();
	out << "(";
	if(eve != NULL){
		out << eve->name() ;
		out << " (" ;
		const vector<const VariableEntry* >* pars = params();
		const VariableEntry* var;
		for (vector<const VariableEntry* >::const_iterator i = pars->begin(); i != pars->end(); ++i) {
			var = *i;
			out << var->name()  ;
			out << "," ;
		} 
		out << " )" ;
	}
	if (cond_ != NULL) {
		out << "| ";
		const ExprNode* c = cond();
		c -> typePrint(out, indent); // what indent to take ?
	}
	out << ")";
	
}


const Type* PrimitivePatNode::typeCheck() const {

// check event param types just like fn invocation
	return NULL;
	

}

void StmtNode::print(ostream& out, int indent) const {
	
       // code to print statements
	if((this)->stmtNodeKind()==StmtNode::StmtNodeKind::EXPR){
		const ExprStmtNode* esn = dynamic_cast<const ExprStmtNode* >(this);
		esn->print(cout, indent);
		
	}
	else if((this)->stmtNodeKind()==StmtNode::StmtNodeKind::IF){
		const IfNode* ifn = dynamic_cast<const IfNode* >(this);
		ifn->print(cout, indent);
	}
	else if((this)->stmtNodeKind()==StmtNode::StmtNodeKind::RETURN){
		const ReturnStmtNode* rsn = dynamic_cast<const ReturnStmtNode* >(this);
		rsn->print(cout, indent);
	}
	else if((this)->stmtNodeKind()==StmtNode::StmtNodeKind::COMPOUND){
		const CompoundStmtNode* csn = dynamic_cast<const CompoundStmtNode* >(this);
		csn->print(cout, indent);
	}
	else cout << " ";
}



void StmtNode::typePrint(ostream& out, int indent) const {
	
       // code to print statements
	if((this)->stmtNodeKind()==StmtNode::StmtNodeKind::EXPR){
		const ExprStmtNode* esn = dynamic_cast<const ExprStmtNode* >(this);
		esn->typePrint(cout, indent);
		
	}
	else if((this)->stmtNodeKind()==StmtNode::StmtNodeKind::IF){
		const IfNode* ifn = dynamic_cast<const IfNode* >(this);
		ifn->typePrint(cout, indent);
	}
	else if((this)->stmtNodeKind()==StmtNode::StmtNodeKind::RETURN){
		const ReturnStmtNode* rsn = dynamic_cast<const ReturnStmtNode* >(this);
		rsn->typePrint(cout, indent);
	}
	else if((this)->stmtNodeKind()==StmtNode::StmtNodeKind::COMPOUND){
		const CompoundStmtNode* csn = dynamic_cast<const CompoundStmtNode* >(this);
		csn->typePrint(cout, indent);
	}
	else cout << " ";
	
}

// create type checking functions for statements now-- most of them return NULL

const Type* StmtNode::typeCheck() const{
	const Type* stmtType = 0;
	if((this)->stmtNodeKind()==StmtNode::StmtNodeKind::EXPR){
		// handled inside header file itself //

		const ExprStmtNode* esn = dynamic_cast<const ExprStmtNode*>(this);
		stmtType = esn->typeCheck();
	}
	else if((this)->stmtNodeKind()==StmtNode::StmtNodeKind::IF){

		const IfNode* ifn = dynamic_cast<const IfNode*>(this);
		stmtType = ifn->typeCheck();
	}
	else if((this)->stmtNodeKind()==StmtNode::StmtNodeKind::RETURN){

		const ReturnStmtNode* rsn = dynamic_cast<const ReturnStmtNode* >(this);
		stmtType = rsn->typeCheck();
	}
	else if((this)->stmtNodeKind()==StmtNode::StmtNodeKind::COMPOUND){

		const CompoundStmtNode* csn = dynamic_cast<const CompoundStmtNode*>(this);
		stmtType = csn->typeCheck();
	}
	
	return stmtType;
}


const Type* ReturnStmtNode::typeCheck() const {
	const Type* t1 = expr_->typeCheck();

	const Type* fType = fun_->type();

	if((Type::isNumeric(fType->tag()) && !(Type::isNumeric(t1->tag()))) || 
			    (!(Type::isNumeric(fType->tag())) && Type::isNumeric(t1->tag()))){
				
					errMsg("Return statement expects a different type");
				
			}
			if((Type::isBool(fType->tag()) && !(Type::isBool(t1->tag()))) || 
			    (!(Type::isBool(fType->tag())) && Type::isBool(t1->tag())))
				{
					errMsg("Return statement expects a different type");
				}
			
			if((fType->tag()==Type::TypeTag::CLASS && t1->tag() != Type::TypeTag::CLASS) ||
			    (fType->tag() != Type::TypeTag::CLASS && t1->tag() == Type::TypeTag::CLASS))
				{
				
					errMsg("Return statement expects a different type");
				}
			
	return NULL;
}



void ValueNode::print(ostream& out, int indent) const {

// code to print ValueNode
	const Value* v = value();
	v->print(out, indent);
	out << " ";

}

void ValueNode::typePrint(ostream& out, int indent) const {

// code to print the type of ValueNode
	const Value* v = value();
	const Type* typ = v->type();
	typ->print(out, indent);
	out << " ";

}





/****************************************************************/
extern const OpNode::OpInfo opInfo[] = {
  // print name, arity, paren_flag, fixity, arg types, out type, constraints
  //
  // Paren_flag -- opnode->print() outputs is surrounded by parenthesis if 
  // this flag is set. As set below, the expression may not print correctly
  // in some rare cases, e.g., ~(b * c) will get printed as ~b * c,
  // which actually corresponds to (~b)*c. To ensure that things get printed
  // correctly all the time, more paren_flags should be set to 1, but this
  // will lead to more clutter in printed output. Basically, what we have done
  // here is to look are expressions by type -- arithmetic, relational, 
  // boolean, bit operations, etc. Within each type, the highest priority 
  // operator is printed without paren. This will work correctly, as long
  // as the language doesn't permit mixing of different types of expressions.
  // But this assumption doesn't always hold, as in the example above. Also,
  // there is an exception to this general approach in the case of unary minus
  // and * -- since (-a)*b and -(a*b) have the same meaning, we can exclude
  // paren for * without an error.
  //
  // Codes for constraints:
  // first character:
  //    N: No additional constraint over what is given by argTypes
  //    I: all arguments must have identical type
  //    S: one of the arguments must have a type that is a supertype of
  //        of all other arguments. All other arguments require a coercion 
  //        operation to be introduced so as to convert their type to S.
  //    s: one of the arguments must have a type that is a subtype of
  //        of all other arguments. 
  //    L: all list arguments (and list output) must have same type. In 
  //        addition, all non-list arguments (and output) must have same 
  //        type as that of elements in these lists
  //    T: all tuple arguments to the function must have same type.
  //    A: (assignment). Type of second argument must be a subtype of
  //       the first argument
  //
  // second character:
  //    O: output type is the same as out type. (In the following cases,
  //        the output type need not be equal to out type, but a subtype
  //        of it.) Since a TypeTag provides complete type information only
  //        for primitive types, `O' is applicable only in this case.
  //    digit: output type is the same as that of the digit'th argument
  //       In this case, a third character may be used, the code for
  //       which is as follows:
  //         'e' denotes that the output is of type alpha, where
  //             the type of digit'th argument is list(alpha)
  //         'l' denotes that the output is of type list(alpha), where
  //             alpha is the type of the digit'th argument.
  //    S: The output type is the same as that of the argument with the
  //        most general type. (Typically used with first character 'S')
  //    s: The output type is the same as that of the argument with the
  //        least general type. (Typically used with first character 'S')
  //    P: The output type is the product of the types of all arguments
  //    p: The output type is a component of the input tuple type. The
  //        following character specifies the component. A digit k specifies
  //        that the component number as k. The character 'a' indicates that
  //        the component number is given by an integer argument to the
  //        operator. The argument number is given by the following digit.
  //        'p' can be used only in conjunction with first character 'P'.
  //    L: Output type is the same as type of list arguments. Can be used
  //        only in conjunction with first character L.
  //    e: Output type is the same as type of element of list arguments. 
  //        Can be used only in conjunction with first character L.
  //
  {OpNode::OpCode::UMINUS, "-",  1, 0, OpNode::OpPrintType::PREFIX, {Type::SIGNED}, Type::SIGNED, "N1"},
  {OpNode::OpCode::PLUS, "+",  2, 1, OpNode::OpPrintType::INFIX, {Type::NUMERIC, Type::NUMERIC}, Type::NUMERIC, "SS"},
  {OpNode::OpCode::MINUS, "-",  2, 1, OpNode::OpPrintType::INFIX, {Type::NUMERIC, Type::NUMERIC}, Type::NUMERIC, "SS"},
  {OpNode::OpCode::MULT, "*",  2, 0, OpNode::OpPrintType::INFIX, {Type::NUMERIC, Type::NUMERIC}, Type::NUMERIC, "SS"},
  {OpNode::OpCode::DIV, "/",  2, 1, OpNode::OpPrintType::INFIX, {Type::NUMERIC, Type::NUMERIC}, Type::NUMERIC, "SS"},
  {OpNode::OpCode::MOD, "%",  2, 1, OpNode::OpPrintType::INFIX, {Type::INTEGRAL, Type::INTEGRAL}, Type::INTEGRAL, "S2"},
  {OpNode::OpCode::EQ, "==", 2, 0, OpNode::OpPrintType::INFIX, {Type::PRIMITIVE, Type::PRIMITIVE}, Type::BOOL, "SO"},
  {OpNode::OpCode::NE, "!=", 2, 0, OpNode::OpPrintType::INFIX, {Type::PRIMITIVE, Type::PRIMITIVE}, Type::BOOL, "SO"},
  {OpNode::OpCode::GT, ">",  2, 0, OpNode::OpPrintType::INFIX, {Type::SCALAR, Type::SCALAR}, Type::BOOL, "SO"},
  {OpNode::OpCode::LT, "<",  2, 0, OpNode::OpPrintType::INFIX, {Type::SCALAR, Type::SCALAR}, Type::BOOL, "SO"},
  {OpNode::OpCode::GE, ">=", 2, 0, OpNode::OpPrintType::INFIX, {Type::SCALAR, Type::SCALAR}, Type::BOOL, "SO"},
  {OpNode::OpCode::LE, "<=", 2, 0, OpNode::OpPrintType::INFIX, {Type::SCALAR, Type::SCALAR}, Type::BOOL, "SO"},
  {OpNode::OpCode::AND, "&&",  2, 1, OpNode::OpPrintType::INFIX, {Type::BOOL, Type::BOOL}, Type::BOOL, "NO"},
  {OpNode::OpCode::OR, "||",  2, 1, OpNode::OpPrintType::INFIX, {Type::BOOL, Type::BOOL}, Type::BOOL, "NO"},
  {OpNode::OpCode::NOT, "!",  1, 0, OpNode::OpPrintType::PREFIX, {Type::BOOL}, Type::BOOL, "NO"}, 
  {OpNode::OpCode::BITNOT, "~",  1, 0, OpNode::OpPrintType::PREFIX, {Type::INTEGRAL}, Type::INTEGRAL, "N1"},
  {OpNode::OpCode::BITAND, "&",  2, 1, OpNode::OpPrintType::INFIX, {Type::INTEGRAL, Type::INTEGRAL}, Type::INTEGRAL, "Ss"},
  {OpNode::OpCode::BITOR, "|",  2, 1, OpNode::OpPrintType::INFIX, {Type::INTEGRAL, Type::INTEGRAL}, Type::INTEGRAL, "SS"},
  {OpNode::OpCode::BITXOR, "^",  2, 0, OpNode::OpPrintType::INFIX, {Type::INTEGRAL, Type::INTEGRAL}, Type::INTEGRAL, "SS"},
  {OpNode::OpCode::SHL, "<<", 2, 1, OpNode::OpPrintType::INFIX, {Type::INTEGRAL, Type::INTEGRAL}, Type::INTEGRAL, "N1"},
  {OpNode::OpCode::SHR, ">>", 2, 1, OpNode::OpPrintType::INFIX, {Type::INTEGRAL, Type::INTEGRAL}, Type::INTEGRAL, "N1"},
  {OpNode::OpCode::ASSIGN, "=",  2, 0, OpNode::OpPrintType::INFIX, {Type::NATIVE, Type::NATIVE}, Type::VOID, "AO"},
  {OpNode::OpCode::PRINT, "print", OpNode::VARIABLE, 1, OpNode::OpPrintType::PREFIX, {Type::NATIVE}, Type::VOID, "NO"},
  {OpNode::OpCode::INVALID, "invalid",            0, 0, OpNode::OpPrintType::PREFIX, {}, Type::ERROR, "NO"}
};

OpNode::OpNode(OpCode op, ExprNode* a1, ExprNode* a2, 
			   int ln, int col, string file):
  ExprNode(ExprNode::ExprNodeType::OP_NODE, NULL, ln,col,file) {
  opCode_ = op;
  if (a1 != NULL) {
	arity_ = 1;
	arg_.push_back(a1);
	if (a2 != NULL) {
	  arity_++;
	  arg_.push_back(a2);
	}
  }
}

OpNode::OpNode(const OpNode &other):
  ExprNode(other) {
  arity_ = other.arity();
  opCode_ = other.opCode();
  for (unsigned int i=0; (i < other.arity()); i++) {
    if (other.arg_[i]) {
      arg_.push_back((other.arg_[i])->clone());
    } 
	else {
      arg_.push_back(NULL);
    }
  }
}

void 
OpNode::print(ostream& os, int indent) const {
	int iopcode = static_cast<int>(opCode_);
  if (opInfo[iopcode].prtType_ == OpNode::OpPrintType::PREFIX) {
	os << opInfo[iopcode].name_;
	if (arity_ > 0) {
	  if (opInfo[iopcode].needParen_) 
		os << '(';
	  for (unsigned i=0; i < arity_-1; i++) {
		if (arg_[i])
		  arg_[i]->print(os, indent);
	    else os << "NULL";
		os << ", ";
	  }
      if (arg_[arity_-1])
		arg_[arity_-1]->print(os, indent);
	  else os << "NULL";
	  if (opInfo[iopcode].needParen_) 
		os << ") ";
	}
  }
  else if ((opInfo[iopcode].prtType_ == OpNode::OpPrintType::INFIX) && (arity_ == 2)) {
	if (opInfo[iopcode].needParen_) 
	  os << "(";
	if(arg_[0])
	  arg_[0]->print(os, indent);
	else os << "NULL";
	os << opInfo[iopcode].name_; 
	if(arg_[1])
	  arg_[1]->print(os, indent);
	else os << "NULL";
	if (opInfo[iopcode].needParen_) 
	  os << ")";
  }
  else internalErr("Unhandled case in OpNode::print");
}



void 
OpNode::typePrint(ostream& os, int indent) const {
	int iopcode = static_cast<int>(opCode_);
  if (opInfo[iopcode].prtType_ == OpNode::OpPrintType::PREFIX) {
	os << opInfo[iopcode].name_;
	if (arity_ > 0) {
	  if (opInfo[iopcode].needParen_) 
		os << '(';
	  for (unsigned i=0; i < arity_-1; i++) {
		if (arg_[i])
		  arg_[i]->typePrint(os, indent);
		  
	    else os << "NULL";
		os << ", ";
	  }
      if (arg_[arity_-1])
		arg_[arity_-1]->typePrint(os, indent);
	  else os << "NULL";
	  if (opInfo[iopcode].needParen_) 
		os << ") ";
	}
  }
  else if ((opInfo[iopcode].prtType_ == OpNode::OpPrintType::INFIX) && (arity_ == 2)) {
	if (opInfo[iopcode].needParen_) 
	  os << "(";
	if(arg_[0])
	  arg_[0]->typePrint(os, indent);
	else os << "NULL";
	os << opInfo[iopcode].name_; 
	if(arg_[1])
	  arg_[1]->typePrint(os, indent);
	else os << "NULL";
	if (opInfo[iopcode].needParen_) 
	  os << ")";
  }
  else internalErr("Unhandled case in OpNode::typeprint");
}

const Type* OpNode::typeCheck() const{
	OpCode opc = opCode();
	if(opc == OpNode::OpCode::PLUS || opc == OpNode::OpCode::MINUS || opc == OpNode::OpCode::MULT || opc == OpNode::OpCode::DIV){
		// determine the arity and return the exprnodes //
		ExprNode* exp1 = arg_[0];
		// call the upper level type checkers !!! //
		const Type* t1 = exp1->typeCheck();
		
		ExprNode* exp2 = arg_[1];
		const Type* t2 = exp2->typeCheck();
		
		// check for type compatibility now-- both of them must be numeric types else wrong//
		if(!(Type::isNumeric(t1->tag())) && !(Type::isNumeric(t2->tag()))){
			errMsg("Incompatible types");
		}
		
		else{
			if(Type::isIntegral(t1->tag()) && Type::isFloat(t2->tag())){
				exp1->coercedType(t2); // int t1 coerced to float t1
				return t2;
			}
			if(Type::isFloat(t1->tag()) && Type::isIntegral(t2->tag())){
				exp2->coercedType(t1); // int t2 coerced to float t2 
				return t1;
			}
			
		}
		// need to stop error propagations-- if t1 is numeric, return else return t2
		// if both of them non-numeric- return INT anyway
		if(!(Type::isNumeric(t1->tag())) && Type::isNumeric(t2->tag())){
			return t2;
		}
		else if(Type::isNumeric(t1->tag()) && !(Type::isNumeric(t2->tag()))){
			return t1;
		}
		else{
			return new Type(Type::TypeTag::INT);
		}
	}
	if(opc == OpNode::OpCode::MOD || opc == OpNode::OpCode::BITAND || opc == OpNode::OpCode::BITXOR || opc == OpNode::OpCode::BITOR ||
	   opc == OpNode::OpCode::SHL || opc == OpNode::OpCode::SHR ){
		// mod takes only integer args - so will BITAND/BITOR/BITXOR etc.. //
		
		ExprNode* exp1 = arg_[0];
		const Type* t1 = exp1->typeCheck();
		
		
		ExprNode* exp2 = arg_[1];
		const Type* t2 = exp2->typeCheck();
		
		// check for type compatibility-only integers now //
		if(!(Type::isIntegral(t1->tag())) && !(Type::isIntegral(t2->tag()))){
			errMsg("Incompatible types");
		}
	        if(!(Type::isIntegral(t1->tag())) && Type::isIntegral(t2->tag())){
			return t2;
		}
		else if(Type::isIntegral(t1->tag()) && !(Type::isIntegral(t2->tag()))){
			return t1;
		}
		else{
			return new Type(Type::TypeTag::INT);
		}
	}
	// relational operators here -- either integer or float args//
	if(opc == OpNode::OpCode::EQ || opc == OpNode::OpCode::NE || opc == OpNode::OpCode::GT || opc == OpNode::OpCode::LT 
		|| opc == OpNode::OpCode::GE || opc == OpNode::OpCode::LE){

			ExprNode* exp1 = arg_[0];
			const Type* t1 = exp1->typeCheck();
			
			ExprNode* exp2 = arg_[1];
			const Type* t2 = exp2->typeCheck();
			// check for type compatibility- int/float //

			if(!(Type::isNumeric(t1->tag())) && !(Type::isNumeric(t2->tag()))){
				errMsg("Incompatible Types");
			}
			else{
				if(Type::isIntegral(t1->tag()) && Type::isFloat(t2->tag())){
					exp1->coercedType(t2); // int t1 coerced to float t1
					return t2;
				}
				if(Type::isFloat(t1->tag()) && Type::isIntegral(t2->tag())){
					exp2->coercedType(t1); // int t2 coerced to float t2 
					return t1;
				}
			}
			if(!(Type::isNumeric(t1->tag())) && Type::isNumeric(t2->tag())){
				return t2;
			}
			else if(Type::isNumeric(t1->tag()) && !(Type::isNumeric(t2->tag()))){
				return t1;
			}
			else{
				return new Type(Type::TypeTag::INT);
			}
		
	}
	// logical operators here -- boolean args//
	if(opc == OpNode::OpCode::AND || opc == OpNode::OpCode::OR){
			ExprNode* exp1 = arg_[0];
			const Type* t1 = exp1->typeCheck();

			ExprNode* exp2 = arg_[1];
			const Type* t2 = exp2->typeCheck();

			// check for type compatibility- boolean
			if(!(Type::isBool(t1->tag())) && !(Type::isBool(t2->tag()))){
				errMsg("Incompatible types");
			}
			if(!(Type::isBool(t1->tag())) && Type::isBool(t2->tag())){
				return t2;
			}
			else if(Type::isBool(t1->tag()) && !(Type::isBool(t2->tag()))){
				return t1;
			}
			else{
				return new Type(Type::TypeTag::BOOL);
			}
	}
	if(opc == OpNode::OpCode::NOT){
			//unary operator
			ExprNode* exp1 = arg_[0];
			const Type* t1 = exp1->typeCheck();
			if(!(Type::isBool(t1->tag()))){
				errMsg("Incompatible Type");
				return new Type(Type::TypeTag::BOOL);
			}
			return t1;
	}
	if(opc == OpNode::OpCode::UMINUS){
			ExprNode* exp1 = arg_[0];
			const Type* t1 = exp1->typeCheck();
			if(!(Type::isNumeric(t1->tag()))){
				errMsg("Incompatible Type");
				return new Type(Type::TypeTag::INT);
			}
			return t1;
		}
	// need to handle assignment expressions also
	if(opc == OpNode::OpCode::ASSIGN){
			//arg(1) is a subtype of arg(0)
			ExprNode* exp1 = arg_[0];
			const Type* t1 = exp1->typeCheck();
			
			ExprNode* exp2 = arg_[1];
			const Type* t2 = exp2->typeCheck();
			const RefExprNode* rexp1 = dynamic_cast<const RefExprNode* >(exp1);
			if(rexp1->symTabEntry()->kind() != SymTabEntry::Kind::VARIABLE_KIND){
					errMsg("LHS must be a variable");
			}
			if((Type::isNumeric(t1->tag()) && !(Type::isNumeric(t2->tag()))) || 
			    (!(Type::isNumeric(t1->tag())) && Type::isNumeric(t2->tag()))){
				
					errMsg("rhs not a subtype of lhs");
				
			}
			if((Type::isBool(t1->tag()) && !(Type::isBool(t2->tag()))) || 
			    (!(Type::isBool(t1->tag())) && Type::isBool(t2->tag())))
				{
					errMsg("rhs not a subtype of lhs");
				}
			
			if((t1->tag()==Type::TypeTag::CLASS && t2->tag() != Type::TypeTag::CLASS) ||
			    (t1->tag() != Type::TypeTag::CLASS && t2->tag() == Type::TypeTag::CLASS))
				{
				
					errMsg("rhs not a subtype of lhs");
				}
			
			
			
			SymTabEntry* ste = stm.currentScope(); 
			if(ste->kind() == SymTabEntry::Kind::BLOCK_KIND){
				// currently inside a rule
				const VariableEntry* ventry = dynamic_cast<const VariableEntry* >(rexp1->symTabEntry());
				if(ventry->varKind() != VariableEntry::VarKind::GLOBAL_VAR){
					errMsg("LHS of assignment in rules must be a global variable");
				}
			}
			return new Type(Type::TypeTag::BOOL); // return BOOL here ...
		}
	else return NULL;
	}



