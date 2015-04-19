%{
import java.io.*;
import java.util.*;
%}
%token CLASS_T INT_T BOOL_T STRING_T IN_T TEL_T
%token INHERIT_T
%token THEN_T ELSE_T FI_T LOOP_T POOL_T
%token NEW_T ISVOID_T LE LT GT GE NE EQ NOT_T
%token OUT_STRING OUT_INT IN_INT
%token TRUE_T FALSE_T INT_CONST LET_T IF_T WHILE_T
%token STR_CONST 
%token ASSIGN
%token <sval> ID TYPE

%type <obj> program class feature_list feature formal_list formal actual_list   expr_list expr
%type <sval> simple_constant INT_CONST STR_CONST typename
%right ASSIGN
%nonassoc GE GT NE LT LE EQ
%right UC
%right UM
%left '+' '-'
%left '*'
%start program
%%

program   : program class {$$= new Program((Program)$1,(Class)$2);}
    | class { $$= new Program((Class)$1);}
    ;

class   : CLASS_T TYPE '{' feature_list '}' {$$ = new Class($2,OBJ,(FeatList)$4);}
    | CLASS_T TYPE INHERIT_T TYPE '{' feature_list '}' {$$ = new Class($2,$4,(FeatList)$6);}
    ;

feature   : ID '(' formal_list ')' ':' typename '{' expr_list  '}' {$$=new MethodFeature($1,$6,(FormalList)$3,(ExprList)$8);}
    | ID '(' ')' ':' typename '{' expr_list  '}'{$$=new MethodFeature($1,$5,(FormalList)null,(ExprList)$7);}
    | ID ':' typename {$$=new Feature($1,$3);}
    | ID ':' typename ASSIGN simple_constant {$$=new Feature($1,$3,$5);}
    | ID ':' STRING_T ASSIGN STR_CONST {$$=new Feature($1,STR,$5);} 
    | ID ':' INT_T '[' ']'  {$$=new Feature($1,ARR);}
    ;

typename  : INT_T {$$=INT;}
    | BOOL_T {$$=BOOL;}
    | STRING_T {$$=STR;}
    | TYPE {$$=$1;}
    ;

simple_constant : INT_CONST {$$=$1;}
    | TRUE_T {$$="True";}
    | FALSE_T {$$="False";}
    ;

formal    : ID ':' typename {$$=new Formal($1,$3);}
    |   ID ':' INT_T '[' ']'{$$=new Formal($1,ARR);}
    
;

expr    : ID ASSIGN  expr = {$$=new Assignment($1,(Expr)$3);}
    | ID '[' expr ']' ASSIGN expr {$$=new ArrayAssignment($1,(Expr)$3,(Expr)$6);}
    | ID '.' ID'(' ')' {$$=new MethodCall($1,$3,null);}
    | ID '.' ID'(' actual_list ')' {$$=new MethodCall($1,$3,(ActList)$5);}
    | ID '(' ')' {$$=new MethodCall("self",$1,null);}
    | ID '(' actual_list ')' {$$=new MethodCall("self",$1,(ActList)$3);}
    | ID {$$=new IDLookup($1);}
    | ID '[' expr ']' {$$=new ArrayLookup($1,(Expr)$3);}
    | IF_T expr THEN_T expr ELSE_T expr FI_T {$$=new If((Expr)$2,(Expr)$4,(Expr)$6);}
    | WHILE_T expr  LOOP_T expr POOL_T {$$=new While((Expr)$2,(Expr)$4);}
    | '{' expr_list '}' {$$=new Sequence((ExprList)$2);}
    | LET_T formal_list  IN_T expr TEL_T {$$=new Let((FormalList)$2,(Expr)$4);}
      | NEW_T TYPE {/*Pretty sure this is an error*/$$=new Constant($2);}
    | NEW_T TYPE '(' ')'{$$=new ClassDec($2,null);}
    | NEW_T TYPE '(' actual_list ')'{$$=new ClassDec($2,(ActList)$4);}
    | NEW_T INT_T '[' expr ']' {$$=new ArrayDec((Expr)$4);}
    | ISVOID_T expr  {$$=new OpExpr(OpExpr.OP.VOID,(Expr)$2);}
    | expr  '+' expr {$$=new OpExpr(OpExpr.OP.PLUS,(Expr)$1,(Expr)$3);}
    | expr  '-' expr {$$=new OpExpr(OpExpr.OP.MINUS,(Expr)$1,(Expr)$3);}
    | expr  '*' expr {$$=new OpExpr(OpExpr.OP.MULT,(Expr)$1,(Expr)$3);}
    | expr  '/' expr {$$=new OpExpr(OpExpr.OP.DIV,(Expr)$1,(Expr)$3);}
    | '~' expr    {$$=new OpExpr(OpExpr.OP.CMP,(Expr)$2);}
    | NOT_T expr    {$$=new OpExpr(OpExpr.OP.NOT,(Expr)$2);}
    | expr NE expr    {$$=new OpExpr(OpExpr.OP.NE,(Expr)$1,(Expr)$3);}
    | expr GT expr    {$$=new OpExpr(OpExpr.OP.GT,(Expr)$1,(Expr)$3);}
    | expr GE expr    {$$=new OpExpr(OpExpr.OP.GE,(Expr)$1,(Expr)$3);}
    | expr LT expr    {$$=new OpExpr(OpExpr.OP.LT,(Expr)$1,(Expr)$3);}
    | expr LE expr    {$$=new OpExpr(OpExpr.OP.LE,(Expr)$1,(Expr)$3);}
    | expr EQ expr  {$$=new OpExpr(OpExpr.OP.EQ,(Expr)$1,(Expr)$3);}
    | '(' expr ')' {$$=new ParenExpr((Expr)$2);}
    | TRUE_T {$$=new Constant(BOOL,"1");}
    | FALSE_T {$$=new Constant(BOOL,"0");}
    | INT_CONST {$$=new Constant(INT,$1);}
    | STR_CONST {$$=new Constant(STR,$1);}
    ;

actual_list :  expr ',' actual_list {$$=new ActList((ActList)$3,(Expr)$1);}
    | expr{$$=new ActList((Expr)$1);}
    ;

expr_list : expr ';' expr_list {$$=new ExprList((ExprList)$3,(Expr)$1);}
    | expr {$$=new ExprList((Expr)$1);}//UncoolAid 3.7
    ;

feature_list  :  feature ';' feature_list {$$=new FeatList((FeatList)$3,(Feature)$1);}
    | {$$=new FeatList();}
    ;

formal_list : formal_list ',' formal {$$=new FormalList((FormalList)$1,(Formal)$3);}
    | formal {$$=new FormalList((Formal)$1);}
    ; 
%%
/* Byacc/J expects a member method int yylex(). We need to provide one
   through this mechanism. See the jflex manual for more information. */

  /* reference to the lexer object */
  private scanner lexer;
  public static Map<String,Class> program;
  public static ArrayList<Class> basicTypes;
  public static Class self;
  public static MethodFeature in_string;
  public static MethodFeature in_int;
  public static MethodFeature out_string;
  public static MethodFeature out_int;
  public static String INT="Int";
  public static String ARR="Int[]";
  public static String STR="String";
  public static String BOOL="Bool";
  public static String OBJ="Object";
  public static String UNK="Unknown";

  /* interface to the lexer */
  private int yylex() {
    int retVal = -1;
    try {
      retVal = lexer.yylex();
    } catch (IOException e) {
      System.err.println("IO Error:" + e);
    }
    return retVal;
  }
  public static scanner lex;
  /* error reporting */
  public void yyerror (String error) {
    System.err.println("Error : " + error + " at line " + lexer.getLine());
    System.err.println("String rejected");
  }

  /* constructor taking in File Input */
  public Parser (Reader r) {
    lexer = new scanner (r, this);
    lex=lexer;
  }

  public static ArrayList<String> errors;
  public static void main (String [] args) throws IOException {
    makeBuiltInTypes();
    errors=new ArrayList<String>();
    Parser yyparser = new Parser(new InputStreamReader(System.in));

    yyparser.yyparse();
    findMain(program);
    for(Class c:program.values()){
      self=c;
      c.typecheckClass();
    }
    if(errors.size()>0)
      flushErrors();
    //else
    {
      writeAssemblyHeader();
      for(Class c:program.values()){
        if(!basicTypes.contains(c)){
          self=c;
          c.compileClass();
        }
      }
      writeMain();
      System.out.println("  .section  .note.GNU-stack,\"\",@progbits");
    }
  }

  private static int dataLabs=3;
  public static String getDataLabel(){
    return ".LC"+dataLabs++;
  }

  private static int ifLabs=0;
  public static String getIfLabel(){
    return ".IF"+ifLabs++;
  }
  
  public static boolean lValue(String expr){
    return isID(expr)||inMemory(expr);
  }

  public static boolean isID(String name){
    return name.matches("[a-z][A-Za-z_0-9]*");
  }

  public static boolean isMemberVariable(String loc){
    return loc.length()>4&&loc.substring(0,4).equals("self");
  }

  public static String getMemberOffset(String selfloc,String self){
    assert(isMemberVariable(selfloc));
    selfloc=selfloc.substring(5);
    return Integer.parseInt(selfloc)+"("+self+")";
  }

  public static boolean isConstValue(String loc){
    return loc.charAt(0)=='$';
  }

  public static boolean isRegister(String loc){
    return loc.charAt(0)=='%';
  }

  public static boolean isOffsetAddress(String loc){
    return loc.indexOf('(')!=-1;
  }

  public static boolean inMemory(String loc){
    return isMemberVariable(loc)||(isOffsetAddress(loc));
  }

  public static void movl(String src,String dest){
    assert(src!=null);
    assert(dest!=null);
    assert(!isMemberVariable(src));
    assert(!isMemberVariable(dest));
    if(!inMemory(src)||!inMemory(dest)){
      System.out.println("  movl "+src+", "+dest);
    }
    else{
      System.out.println("  movl "+src+", %eax");
      System.out.println("  movl %eax, "+dest);
    }
  }

  public static void writeAssemblyHeader(){
    System.out.println("  .section  .rodata.str1.1,\"aMS\",@progbits,1");
    System.out.println("  .data");
    System.out.println(".LC0:");
    System.out.println("  .string \"%d\"");
    System.out.println(".LC1:");
    System.out.println("  .string \"%d \"");
    System.out.println(".LC2:");
    System.out.println("  .string \"%s \"");
    System.out.println("  .align 4");
    System.out.println("  .type _uncool_input,@object");
    System.out.println("  .size _uncool_input,4");
    System.out.println("_uncool_input:");
    System.out.println("  .long 0");
  }

  public static void writeMain(){
      MethodFeature.methodEntry("main");
      System.out.println("call __Main_init");
      movl("%eax","(%esp)");
      System.out.println("call __Main_main");
      MethodFeature.methodExit("main");
  }

  public static void logError(int lineno,String message){
    errors.add("line "+lineno+": "+message);
  }
  public static void flushErrors(){
    errors = new ArrayList<String>(new LinkedHashSet<String>(errors));
    Comparator<String> lineOrder = new Comparator<String>() {
            public int compare(String o1, String o2) {
              o1=o1.substring(o1.indexOf("e")+2,o1.indexOf(":"));
              o2=o2.substring(o2.indexOf("e")+2,o2.indexOf(":"));
              int i=Integer.parseInt(o1);
              int j=Integer.parseInt(o2);
                return new Integer(i).compareTo(j);
            }
        };
    Collections.sort(errors,lineOrder);
    for(String error:errors){
      System.err.println(error);
    }
    System.err.println("Errors found: "+errors.size());
    errors.clear();
  }

  public static void findMain(Map<String,Class> prog){
    boolean found=false;
    Class main=prog.get("Main");  
    if(main!=null){
      for(MethodFeature m:main.getMethods()){
        if(m.args==null&&m.name.equals("main")){
          found=true;
        }
      }
    }
    if(!found){
      System.out.println("Main.main() not found");
    }
  }

  public static void makeBuiltInTypes(){
    //TODO Should probably refactor
    program=new HashMap<String,Class>();
    basicTypes=new ArrayList<Class>();
    basicTypes.add(new Class(INT,null,null));
    basicTypes.add(new Class(ARR,null,null));
    basicTypes.add(new Class(STR,null,null));
    FeatList abort=new FeatList();
    abort.features.add(new MethodFeature("abort",OBJ,null,
      new ExprList(new IDLookup("self"))));
    in_int=new MethodFeature("in_int",INT,null,
      new ExprList(new Constant(INT)));
    in_string=new MethodFeature("in_string",STR,null,
      new ExprList(new Constant(STR)));
    out_int=new MethodFeature("out_int",INT,
      new FormalList(new Formal("x",INT)),
      new ExprList(new Constant(INT)));
    out_string=new MethodFeature("out_string",INT,
      new FormalList(new Formal("x",STR)),
      new ExprList(new Constant(INT)));
    basicTypes.add(new Class(OBJ, null,abort)); 
  }

  public static class Program{
    public Program(Program o1, Class o2){

    }
  
    public Program(Class o1){

    }
  }

  public static class Class{
    public String name;
    public String parent;
    public ArrayList<Feature> features;
    public final int lineno;
    public Class(String ClassName, String Parent, FeatList o3){
      name=ClassName;
      parent=Parent;
      if(o3 ==null){
        features=new ArrayList<Feature>();
      }
      else{
        features=o3.features;
      }
      if(lex!=null)//Special case for builtins
        lineno=lex.getLine()+1;
      else{
        lineno=0;
      }
      addToProgram(program);
    }

    public int getLine(){
      return lineno;
    }

    public void addToProgram(Map<String,Class> p){
      if(!p.containsKey(name)){
        p.put(name,this);
      }
      else{
        logError(getLine(),"Duplicate class "+name);
      }
    }

    public String toString(){
      String retVal="Class named "+name;
      if(parent!=null)
        retVal+=" with parent "+parent;
      if(features!=null){
        for(Feature x : features){
          retVal+="\n";
          retVal+=x.toString();
        }
      }
      return retVal;
    }

    public int getSize(){
      Map<String,String> attr=getAttributes();
      int size=attr.values().size()*4;
      return size;
    }

    public MethodFeature getMethod(String method){
      MethodFeature func=getMethodDict().get(method);
      if(func==null&&parent!=null){
        if(null!=getSuper()){
          func=getSuper().getMethod(method);
        }
      }
      return func;
    }

    public Class getSuper(){
      //People with an undefined parent are still Objects
      //DEBUG What about INT,String,BOOL, and INT[]?
      if(name.equals(OBJ))
        return null;
      Class par=program.get(parent);
      if(par==null&&parent!=null){
        par=program.get(OBJ);
      }
      return par;
    }

    public void typecheckClass(){
      if(parent!=null&&null==program.get(parent)){
        logError(lineno,"Class "+name+" inherits from undefined parent "+parent+".");
      }
      Map<String,String> state=getAttributes();
      state.put("self",self.name);
      state.putAll(getAttributes());
      for(MethodFeature mf:getMethods()){
        mf.typecheck(state);
      }
    }
    public Map<String,String> getAttributes(){
      Map<String,String> tReturn;
      Class supe=getSuper();
      if(parent==null){
        return new HashMap<String,String>();
      }
      else{
        tReturn=supe.getAttributes();
      }
      getClassAttributes(tReturn);
      return tReturn;
    }

    public Map<String,String> getClassAttributes(Map<String,String> tReturn){
      for(Feature f:features){
        if(f instanceof Feature && !(f instanceof MethodFeature)){
          if(tReturn.get(f.name)!=null){
            logError(f.lineno,"Duplicate attribute "+f.name+" in class "+name+".");
          }
          tReturn.put(f.name,f.type);
        }
      }
      return tReturn;
    }
    public Map<String,MethodFeature> getMethodDict(){
      Map<String,MethodFeature> tReturn=new HashMap<String,MethodFeature>();
      for(Feature f:features){
        if(f instanceof MethodFeature){
          tReturn.put(f.name,(MethodFeature)f);
        }
      }
      return tReturn;
    }
    public ArrayList<MethodFeature> getMethods(){
      return new ArrayList<MethodFeature>(getMethodDict().values());
    }

    public ArrayList<String> getMethodNames(){
      return new ArrayList<String>(getMethodDict().keySet());
    }

    public boolean isSubtypeOf(String type){
      String[] topTypes={ARR,BOOL,INT,OBJ,STR};
      if(name.equals(type))
        return true;
      if(type.equals(parent))
        return true;
      else if(Arrays.asList(topTypes).contains(name))
        return false;
      else
        return getSuper().isSubtypeOf(type);
    }

    public String commonType(String class1){
      //NOTE Not actually used.
      return commonType(class1,name);
    }

    public static String commonType(String class1,String class2){
      if(class1.equals(class2)){
        return class1;
      }
      String[] specialTypes={ARR,BOOL,INT,STR,UNK};
      if(Arrays.asList(specialTypes).contains(class1)||
        Arrays.asList(specialTypes).contains(class2))
      {
        return "N/A";
      }
      if(class1.equals(class2))
        return class1;
      Class first=program.get(class1);
      Class secnd=program.get(class2);
      if(first.isSubtypeOf(class2)){
        return class2;
      }
      if(secnd==null){
        System.out.println(class2);
      }
      if(secnd.isSubtypeOf(class1)){
        return class1;
      }
      String supeName=first.getSuper().name;
      return commonType(class2,supeName);
    }

    public void compileFeatures(){
      System.out.println("  .data");
      for(Feature f:features){
        assert(name!=null);
        f.setClass(name);
      }
      for(Feature f:features){
        if(f instanceof Feature && !(f instanceof MethodFeature)){
          f.compile();
        }
      }
    }

    public void compileClass(){
      System.out.println("# class "+name);
      compileFeatures();
      compileMethods();
    }

    public void loadDefaults(){
      ArrayList<Feature> fields=getStructFields();
      int offset=0;
      for(Feature field:fields){
        if(!field.value.equals("$0")){ 
          String label="$"+field.value;
          if(field.type.equals(STR))
            label="$"+field.getTag();
          movl(label,offset+"(%eax)");
        }
        offset+=4;
      }
    }

    public void compileInit(LookupTable alloc){
      MethodFeature func=getMethodDict().get("init");
      MethodFeature.methodEntry("__"+name+"_init");
      int size=getSize();
      movl("$"+size,"(%esp)");
      System.out.println("  call malloc");
      movl("%eax","12(%ebp)");
      loadDefaults();
      if(func!=null){
        compileInitBody(func,alloc);
      }
      MethodFeature.methodExit("__"+name+"_init");
    }

    public void compileInitBody(MethodFeature func,LookupTable alloc){
        func.loadArgs(alloc);
        //alloc.put("self","%ecx");
        func.methodBody(alloc);
        alloc.exitScope();
    }

    public void compileMethods(){
      LookupTable lookupTable=getLookupTable();
      System.out.println("  .text");
      compileInit(new LookupTable(lookupTable));
      for(MethodFeature f:getMethods()){
        assert(f.classname!=null);
        if(!f.name.equals("init")){
          f.compile(new LookupTable(lookupTable));
        }
      }
    }

    public ArrayList<Feature> getStructFields(){
      ArrayList<Feature> tReturn;
      Class supe=getSuper();
      if(supe!=null)
        tReturn=supe.getStructFields();
      else
        tReturn=new ArrayList<Feature>();
      for(Feature f:features){
        if(f instanceof Feature && !(f instanceof MethodFeature)){
          tReturn.add(f);
        }
      }
      return tReturn;
    }

    public LookupTable getLookupTable(){
      ArrayList<Feature> fields=getStructFields();
      LookupTable tReturn=new LookupTable();
      Map<String,String> ourAttr=new HashMap<String,String>();
      int offset=0;
      for(Feature field:fields){
        ourAttr.put(field.name,"self+"+offset);
        offset+=4;
      }
      assert(offset==getSize());
      tReturn.enterScope(ourAttr);
      return tReturn;
    }
  }

  public static class FeatList{
    public ArrayList<Feature> features;
    public FeatList(FeatList o1, Feature o2){
      features=o1.features;
      if(o2!=null)
        features.add(o2);
    }
    public FeatList(){
      features=new ArrayList<Feature>();
    }

    public String toString(){
      String retVal="";
      for(int x=0;x<features.size()-1;x++){
        retVal+=features.get(x).toString()+",";
      }     
      if(features.size()>0){
        retVal+=features.get(features.size()-1).toString();
      }
      return retVal;
    }
  }

  public static String constType(String value){
    if(value.equals("True"))
      return BOOL;
    if(value.equals("False"))
      return BOOL;
    if(value.contains("\""))
      return STR;
    else
      try{
        Integer.parseInt(value); 
        return INT;
      }
      catch(Exception e){
        return UNK;
      }

  }

  public static class Feature{
    public String name;
    public String type;
    protected String classname;
    public final int lineno;
    public String value;
    public Feature(String name,String type){
      this.name=name;
      this.type=type;
      if(lex!=null)//Special case for builtins
        lineno=lex.getLine()+1;
      else{
        lineno=0;
      }
      if(type.equals(STR))
        this.value="";
      else
        this.value="0";
    }
    public Feature(String name,String type, String value){
      assert(name!=null&&type!=null);
      this.name=name;
      this.type=type;
      if(lex!=null)//Special case for builtins
        lineno=lex.getLine()+1;
      else{
        lineno=0;
      }
      if(!type.equals(constType(value))){
        logError(lineno,"Initializing "+type+" "+name+
            " with "+value);
      }
      if(value==null&&type.equals(STR))
        this.value="";
      else if(value==null)
        this.value="0";
      else
        this.value=value;
    }

    public void setClass(String classname){
      this.classname=classname;
    }

    public String toString(){
      String retVal=name;
      retVal+=":"+type;
      return retVal;
    }

    public void compile(){
      String tag=getTag();
      System.out.println("  .align 4");
      System.out.println("  .type "+tag+", @object");
      System.out.println("  .size "+tag+",4");
      System.out.println(tag+":");
      if(type.equals(STR))
        System.out.println("  .string "+value);//value
      else
        System.out.println("  .long "+value);
    }

    public String getTag(){
      return "_"+classname+"_"+name;
    }
  }

  public static class MethodFeature extends Feature{
    public final FormalList args;
    public final ExprList expr;
    public MethodFeature(String name, String type,
               FormalList args,ExprList expr){
      super(name,type);
      this.args=args;
      this.expr=expr;
    }

    public String toString(){
      String retVal=name;
      retVal+="(";
      if(args!=null)
      {
        for(Formal f : args.formals)
        {
          retVal+=f.toString();
          if(f!=args.formals.get(args.formals.size() - 1))
          {
            retVal+=",";
          }
        }
      }
      retVal+= ")";
      retVal+=":"+type;
      return retVal;
    }

    public ArrayList<Formal> getDefs(){
      ArrayList<Formal> tReturn;
      if(args==null||args.formals==null)
        tReturn=new ArrayList<Formal>();
      else
        tReturn=args.formals;
      return tReturn;
    }

    public Expr getBody(){
      return new Sequence(expr);
    }

    public String typecheck(Map<String,String> typeMap){
      Map<String,String> methodState=new HashMap<String,String>(typeMap);
      ArrayList<String> argNames=new ArrayList<String>();
      ArrayList<Formal> args=getDefs();
      for(Formal arg:args)
      {
        //UncoolAid 2.1.2 says that method params hide attrbiutes.
        if(argNames.contains(arg.id))
        {
          logError(arg.lineno,"Duplicate "+arg.id+" in "+name+" args.");
        }
        argNames.add(arg.id);
        methodState.put(arg.id,arg.type);
      }
      String ty= getBody().typecheck(methodState);
      if(!ty.equals(type))
      {
        logError(lineno,toString()+" returns "+ty+" declares "+type);
      }
      setClass(self.name);
      return type;
    }
    
    public void compile(LookupTable alloc){
      String tag=getTag();
      methodEntry(tag);
      loadArgs(alloc);
      methodBody(alloc);
      alloc.exitScope();
      methodExit(tag);
    }

    public void methodBody(LookupTable alloc){
      String reg=expr.compile(alloc);
      movl(reg,"%eax");
    }
    
    public void loadArgs(LookupTable alloc){
      Map<String,String> loadedArgs=new HashMap<String,String>();
      loadedArgs.put("self","12(%ebp)");
      int offset=16;
      if(args!=null&&args.formals!=null){
        for(Formal arg:args.formals){
          loadedArgs.put(arg.id,offset+"(%ebp)");
          offset+=4;
        }
      }
      alloc.enterScope(loadedArgs);
    }

    public String getTag(){
      if(classname==null){
        System.err.println(lineno+" "+name);
      }
      assert(classname!=null);
      assert(name!=null);
      /*if(classname.equals("Main")&&name.equals("main"))
        return "main";
      else*/
        return "__"+classname+"_"+name;
    }

    public static void methodEntry(String tag){
      System.out.println("  .globl "+tag);
      System.out.println("  .type "+tag+", @function");
      System.out.println(tag+":");
      System.out.println("  leal 4(%esp), %ecx");
      System.out.println("  andl $-16, %esp");
      System.out.println("  pushl -4(%ecx)");
      System.out.println("  pushl %ebp");
      movl("%esp","%ebp");
      System.out.println("  pushl %ecx");
      System.out.println("  subl $128,%esp");
      movl("%ebx","-16(%ebp)");
      movl("%esi","-20(%ebp)");
      movl("%edi","-24(%ebp)");
      System.out.println("");
    }

    public static void methodExit(String tag){
      movl("-16(%ebp)","%ebx");
      movl("-20(%ebp)","%esi");
      movl("-24(%ebp)","%edi");
      System.out.println("  addl $128,%esp");
      System.out.println("  popl %ecx");
      System.out.println("  popl %ebp");
      System.out.println("  leal -4(%ecx), %esp");
      System.out.println("  ret");
      System.out.println("  .size "+tag+", .-"+tag);
      System.out.println("");
    }
  }

  public static class Formal{
    public final String id;
    public final String type;
    public final int lineno;
    public Formal(String ID,String Type){
      id=ID;
      type=Type;
      if(lex!=null)//Special case for builtins
        lineno=lex.getLine()+1;
      else{
        lineno=0;
      }
    }
    public String toString(){
      return id+":"+type;
    }
  }

  public static class FormalList{
    public ArrayList<Formal> formals;
    public FormalList(FormalList o1, Formal o2){
      formals=o1.formals;
      formals.add(o2);
    }
    public FormalList(Formal o1){
      formals=new ArrayList<Formal>();
      formals.add(o1);
    }
    public Map<String,Formal> getDefinitions(){
      Map<String,Formal> defs=new HashMap<String,Formal>();
      for(Formal f:formals)
      {
        defs.put(f.id,f);
      }
      return defs;
    }

    public int size(){
      return formals.size();
    }

  }

  public static class LookupTable{
    private static String[] registers={"%ebx","%ecx","%edx","%esi","%edi"};
    ArrayList<Map<String,String>> scopes;
    public LookupTable(){
      scopes=new ArrayList<Map<String,String>>();
    } 
    public LookupTable(Map<String,String> state){
      scopes=new ArrayList<Map<String,String>>();
      scopes.add(state);
    }

    public LookupTable(LookupTable src){
      scopes=new ArrayList<Map<String,String>>();
      for(Map<String,String> scope:src.scopes){
        scopes.add(new HashMap<String,String>(scope));
      }
    }

    public void enterScope(Map<String,String> newScope){
      scopes.add(0,newScope); 
    }

    public void exitScope(){
      scopes.remove(scopes.get(0));
    }

    public Collection<String> getLocsInUse(){
      Collection<String> locsInUse=new ArrayList<String>();
      for(Map<String,String> scope:scopes){
        locsInUse.addAll(scope.values());
      }
      //TODO FIXME 
      //assert(locsInUse.size()==new HashSet<String>(locsInUse).size());
      return locsInUse;
    }

    public String loadMemberVariable(String loc){
      System.out.println("  #Loading member variable at "+loc);
      IDLookup self=new IDLookup("self");
      String selfreg=self.compile(this);
      return getMemberOffset(loc,selfreg);
    }

    public String loadIntoRegister(String loc){
      if(inMemory(loc)){
        String var=locToVar(loc);
        Map<String,String> scope=getScope(var);
        String dest=getRegister();
        if(isMemberVariable(loc)){
          loc=loadMemberVariable(loc);
        }
        movl(loc,dest);
        scope.put("RVAL"+var,dest);
        loc=dest;
        getLocsInUse();
      }
      return loc;
    }
    
    public boolean isTemp(String vName){
      return vName.contains("RVAL");
    }

    private void printScope(){
      int count=scopes.size();
      for(Map<String,String> scope:scopes){
        System.err.println("Scope "+count);
        for(Map.Entry<String,String> ent:scope.entrySet())
          System.err.println(ent);
        count--;
      }
    }

    public String moveToTemp(String regName){
      String vName=locToVar(regName);
      if(!isTemp(vName)){
        String tempReg=getRegister();
        movl(regName,tempReg);
        scopes.get(0).put("RVAL"+System.nanoTime(),tempReg);
        regName=tempReg;
      }
      getLocsInUse();
      String newReg=loadIntoRegister(regName);
      if(!Arrays.asList(registers).contains(newReg)){
        System.err.println(newReg+" "+regName);
        assert(false);
      }
      return newReg;
    }

    public void clearTemps(){
      Map<String,String> tidy=new HashMap<String,String>();
      for(Map.Entry<String,String> ent:scopes.get(0).entrySet()){
        if(!isTemp(ent.getKey())){
          tidy.put(ent.getKey(),ent.getValue());
        }
      }
      scopes.set(0,tidy);
    }

    public void flushTemps(){
      for(Map.Entry<String,String> ent:scopes.get(0).entrySet()){
        if(isTemp(ent.getKey())&&!inMemory(ent.getValue())){
          String st=getStackSpot();
          movl(ent.getValue(),st);
          ent.setValue(st);
        }
      }
      getLocsInUse();
    }

    public boolean isVarAttribute(String vName){
      for(Map<String,String> scope:scopes){
        if(scope.get(vName)!=null){
          String loc=scope.get(vName);
          return isAttributeLoc(loc);
        }
      }
      return false;
    }

    public boolean isAttributeLoc(String loc){
      if(loc.charAt(0)=='_')
        return true;
      if(loc.charAt(0)=='$'&&loc.charAt(0)=='_')
        return true;
      return false; 
    }

    public Map<String,String> getScope(String var){
      for(Map<String,String> scope:scopes){
        if(scope.get(var)!=null)
          return scope;
      }
      System.err.println("Fatal Error: "+var+" not in any scope");
      printScope();
      System.err.println("Gonna die now");
      throw new RuntimeException();
    }

    public void put(String name,String loc){
      scopes.get(0).put(name,loc);
      getLocsInUse();
    }

    public String varToLoc(String id){
      String loc=getMemLoc(id);
      if(loc==null){
        System.err.println("could not find "+id);
        printScope(); 
        System.err.println("could not find "+id);
        assert(false);
      }
      return loc;
    }
  
    public String getMemLoc(String id){
      for(Map<String,String> scope:scopes){
        if(scope.get(id)!=null){
          return scope.get(id);
        }
      }
      return null;
    }
    
    public String locToVar(String id){
      for(Map<String,String> scope:scopes){
        for(Map.Entry<String,String> ent:scope.entrySet()){
          if(id.equals(ent.getValue())){
            return ent.getKey();
          }
        }
      }
      System.err.println("Nothing is at "+id);
      throw new RuntimeException();
    }

    public void reassign(String dest,String name){
      assert(isID(name));
      getScope(name).put(name,dest);
    }
    
    private int vicCount=0; 
    public String getRegister(){
      String reg=getUnusedRegister();
      if(reg!=null){return reg;}
      flushTemps();
      reg=getUnusedRegister();
      if(reg!=null){return reg;}
      System.err.println("random vic to get reg");
      String victim = registers[vicCount];
      vicCount++;
      vicCount=vicCount%registers.length;
      saveToStack(victim);
      return victim;
    }

    public String getUnusedRegister(){
      Collection<String> inuse=getLocsInUse();
      for(String reg:registers){
        if(!inuse.contains(reg)){
          return reg;
        }
      }
      return null;
    }
    
    public void saveToStack(String reg){
      String dest=getStackSpot();
      String var=locToVar(reg);
      assert(getScope(var).get(var).equals(reg));
      movl(reg,dest);
      getScope(var).put(var,dest);
    }

    public String getStackSpot(){
      int offset=-28;
      Collection<String> inuse=getLocsInUse();
      while(inuse.contains(offset+"(%ebp)"))
        offset-=4;
      return offset+"(%ebp)";
    }
  }

  public interface Expr{
    public String toString();
    public String typecheck(Map<String,String> typeMap);
    public String compile(LookupTable alloc);
    public int getLine();
    public String getTag();
  }

  public static class Assignment implements Expr{
    final String id;
    final Expr expr;
    final int lineno;
    public Assignment(String id,Expr expr){
      this.id=id;
      this.expr=expr;
      lineno=lex.getLine()+1;
    }
    public String toString(){
      return id+"<-"+expr.toString();
    }
    public int getLine(){
      return lineno;
    }
    public String typecheck(Map<String,String> typeMap){
      String t1=typeMap.get(id);
      String t2=expr.typecheck(typeMap);
      if(t1==null){
        logError(lineno,id+" undefined in assignment.");
      }
      else if(!t1.equals(t2)){
        logError(lineno,"Assigning "+t2+ " to "
            +t1+" "+id+ ".");
      }
      if(t2==null)
        t2=UNK;
      return t2;//UnCoolAid 3.4
    }

    public String compile(LookupTable alloc){
      String src=expr.compile(alloc);
      String dest=alloc.varToLoc(id);
      System.out.println("  #Line "+lineno+": Assignment to "+id);
      if(inMemory(dest)){
        if(isMemberVariable(dest)){
          dest=alloc.loadMemberVariable(dest);
        }
        movl(src,dest);
      }
      else{
        dest = alloc.getStackSpot();
        movl(src,dest);
        alloc.put(id,dest);
      }
      return src;
    }

    public String getTag(){
      return id;
    }
  }

  public static class ParenExpr implements Expr{
    final Expr expr;
    public ParenExpr(Expr expr){
      this.expr=expr;
    }
    public String toString(){
      return "("+expr.toString()+")";
    }

    public int getLine(){
      return expr.getLine();
    }
    public String typecheck(Map<String,String> typeMap){
      return expr.typecheck(typeMap);
    }

    public String compile(LookupTable alloc){
      return expr.compile(alloc);
    }
    
    public String getTag(){
      return expr.getTag();
    }
  }

  public static class ArrayAssignment implements Expr{
    final String name;
    final Expr index;
    final Expr expr;
    final int lineno;
    public ArrayAssignment(String name,Expr index,Expr expr){
      this.name=name;
      this.index=index;
      this.expr=expr;
      lineno=lex.getLine()+1;
    }

    public String toString(){
      return name+"["+index.toString()+"]<-"+expr.toString();
    }

    public int getLine(){
      return lineno;
    }
    public String typecheck(Map<String,String> typeMap){
      String t1=typeMap.get(name);
      if(!ARR.equals(t1)){
        logError(lineno,"Indexing into non-array "+name+".");
      }
      String t2=index.typecheck(typeMap);
      if(!INT.equals(t2)){
        logError(index.getLine(),"Array index is non-int.");
      }
      String t3=expr.typecheck(typeMap);
      if(!INT.equals(t3)){
        logError(expr.getLine(),"Array assignment is non-int.");
      }
      return t3;
    }

    public String compile(LookupTable alloc){
      //TODO DEBUG
      String rExpr=expr.compile(alloc);
      String vExpr=alloc.locToVar(rExpr);
      String rInd=index.compile(alloc);
      String vInd=alloc.locToVar(rInd);
      //Load v2 and v1 into registers
      rExpr=alloc.loadIntoRegister(alloc.varToLoc(vExpr));
      rInd=alloc.loadIntoRegister(alloc.varToLoc(vInd));
      String rName=alloc.loadIntoRegister(alloc.varToLoc(name));
      System.out.println("  #Line "+lineno+": array assignment to "+name);
      movl(rExpr,"0("+rName+","+rInd+",4)");
      return rExpr;
    }
    public String getTag(){
      throw new RuntimeException();
    }
  }

  public static class IDLookup implements Expr{
    final String id;
    final int lineno;
    String type;
    public IDLookup(String id){
      this.id=id;
      if(lex!=null)//Special case for builtins
        lineno=lex.getLine()+1;
      else{
        lineno=0;
      }
      if(id.equals("al")){
        System.err.println(lineno);
        assert(false);
      }
    }

    public String toString(){
      return id;
    }

    public int getLine(){
      return lineno;
    }

    public String typecheck(Map<String,String> typeMap){
      String ty=typeMap.get(id);
      if(ty==null){
        logError(getLine(),id +" is not defined");
        ty=UNK;
      }
      type=ty;
      return ty;
    }   

    public String compile(LookupTable alloc){
      String spot=alloc.varToLoc(id);
      if(inMemory(spot)||spot.equals("$0")){
        String reg=alloc.getRegister();
        if(isMemberVariable(spot)){
          spot=alloc.loadMemberVariable(spot);
        }
        movl(spot,reg);
        spot=reg;
      }
      assert(!isMemberVariable(spot));
      assert(isID(id));
      alloc.put("RVAL"+id,spot);
      return spot;
    }
    public String getTag(){
      throw new RuntimeException();
    }
  }
  

  public static class ArrayLookup implements Expr{
    final String name;
    final Expr index;
    final int lineno;
    String tag;
    public ArrayLookup(String name,Expr index){
      this.name=name;
      this.index=index;
      lineno=lex.getLine()+1;
    }

    public String toString(){
      return name+"["+index.toString()+"]";
    }
    public int getLine(){
      return lineno;
    }
    public String typecheck(Map<String,String> typeMap){
      String ty=typeMap.get(name);
      if(ty==null){
        System.out.println(name +"is not defined");
      }
      else if(!ty.equals(ARR)){
        logError(getLine(),"Indexing into non array "+name);
      }
      return INT;
    }   

    public String compile(LookupTable alloc){
      String rInd=alloc.loadIntoRegister(index.compile(alloc));
      String rName=alloc.loadIntoRegister(alloc.varToLoc(name));
      String rResult=alloc.getRegister();
      System.out.println("  #Line "+lineno+": array lookup from "+name);
      movl("0("+rName+","+rInd+",4)",rResult);
      alloc.put(getTag(),rResult);
      return rResult;
    }

    public String getTag(){
      if(tag==null)
        tag="RVAL"+name+"[#}"+System.nanoTime();
      return tag;
    }
  }

  public static class If implements Expr{
    final Expr guard;
    final Expr ifcase;
    final Expr elsecase;
    final int lineno;
    public If(Expr guard, Expr ifcase, Expr elsecase){
      this.guard=guard;
      this.ifcase=ifcase;
      this.elsecase=elsecase;
      lineno=lex.getLine()+1;
    }

    public String toString(){
      String retVal="if "+guard.toString();
      retVal+=" then "+ifcase.toString();
      retVal+=" else "+elsecase.toString()+" fi";
      return retVal;
    }

    public int getLine(){
      return lineno;
    }
    public String typecheck(Map<String,String> typeMap){
      String t0=guard.typecheck(typeMap);
      if(!t0.equals(BOOL)){
        logError(lineno,"If with non bool factor");
      }
      String t1=ifcase.typecheck(typeMap);
      String t2=elsecase.typecheck(typeMap);
      String comType=Class.commonType(t1,t2); 
      if(comType.equals("N/A")){
        logError(getLine(),"Then and else have different types");
        return UNK;
      }
      return comType;
    }

    public String compile(LookupTable alloc){
      String label=getIfLabel();
      System.out.println("  #Line "+lineno+" IF statement");
      String test=guard.compile(alloc);
      System.out.println("  testl "+test+","+test);
      System.out.println("  je "+label+"Else");
      LookupTable elseAlloc=new LookupTable(alloc);
      String ret=ifcase.compile(alloc);
      movl(ret,"%eax");
      System.out.println("  jmp "+label+"End");
      System.out.println(label+"Else:");
      ret=elsecase.compile(elseAlloc);
      movl(ret,"%eax");
      System.out.println(label+"End:");
      return "%eax";
    }
    public String getTag(){
      throw new RuntimeException();
    }
  }

  public static class Sequence implements Expr{
    final ExprList seq;
    final int lineno;
    public Sequence(ExprList el){
      seq=el;
      if(el==null)
        throw new RuntimeException();
      lineno=lex.getLine()+1;
    }

    public String toString(){
      String retVal="{\n";
      for(Expr e:seq.exprs){
        retVal+=e.toString()+";\n";
      }
      retVal+="}";
      return retVal;
    }
    public int getLine(){
      return lineno;
    }
    public String typecheck(Map<String,String> typeMap){
      String ty=UNK;
     for(Expr e:seq.exprs){
        ty=e.typecheck(typeMap);
      }
      return ty;
    }

    public String compile(LookupTable alloc){
      return seq.compile(alloc);
    }
    public String getTag(){
      throw new RuntimeException();
    }
  }

  public static class While implements Expr{
    final Expr guard;
    final Expr expr;
    final int lineno;
    private static int whileLabs=0;
    public While(Expr guard,Expr expr){
      this.guard=guard;
      this.expr=expr;
      lineno=lex.getLine()+1;
    }

    public String toString(){
      String retVal="While";
      retVal+="("+guard.toString()+")";
      retVal+=expr.toString();
      return retVal;
    }
    public int getLine(){
      return lineno;
    }
    public String typecheck(Map<String,String> typeMap){
      String ty=guard.typecheck(typeMap);
      if(!ty.equals(BOOL)){
        logError(guard.getLine(),"While guard not Bool.");
      }
      expr.typecheck(typeMap);
      return INT;//UncoolAid 3.6
    }
    
    public static String getWhileLabel(){
      return ".WH"+whileLabs++;
    }

    public String compile(LookupTable alloc){
      String label=getWhileLabel();
      System.out.println(label+":");
      String reg=guard.compile(alloc);
      System.out.println("  testl "+reg+","+reg);
      System.out.println("  je "+label+"Exit");
      expr.compile(alloc);
      System.out.println("  jmp "+label);
      System.out.println(label+"Exit:");
      movl("$0","%eax");
      return "%eax";
    }
    public String getTag(){
      throw new RuntimeException();
    }
  }

  public static class Let implements Expr{
    final FormalList defs;
    final Expr body;
    final int lineno;
    public Let(FormalList defs,Expr body){
      this.defs=defs;
      this.body=body;
      lineno=lex.getLine()+1;
    }

    public String toString(){
      String retVal="Let ";
      retVal+=defs.toString();
      retVal+=" in ";
      retVal+=body.toString();
      return retVal;
    }
    public int getLine(){
      return lineno;
    }
    public String typecheck(Map<String,String> typeMap){
      Map<String,String> newState=new HashMap<String,String>(typeMap);
      Map<String,Formal> ourdefs=defs.getDefinitions();
      for(String id:ourdefs.keySet()){
        Formal f=ourdefs.get(id);
        //UncoolAid 3.8  says this is cool.
        newState.put(id,f.type);
      }
      return body.typecheck(newState);
    }

    public void loadDefs(LookupTable alloc){
      Set<String> params=defs.getDefinitions().keySet();
      Map<String,String> loadedDefs=new HashMap<String,String>();
      for(String param:params){
        loadedDefs.put(param,"$0");
      }
      alloc.enterScope(loadedDefs);
    }

    public String compile(LookupTable alloc){
      System.out.println("  #Line "+lineno+": let block");
      loadDefs(alloc);
      System.out.println("  #Let body");
      String retReg=body.compile(alloc);
      alloc.exitScope();
      return retReg;
    }

    public String getTag(){
      throw new RuntimeException();
    }
  }

  public static class ArrayDec implements Expr{
    final Expr size;
    final int lineno;
    String tag;
    public ArrayDec(Expr size){
      this.size=size;
      lineno=lex.getLine()+1;
    }

    public String toString(){
      String retVal="new INT[";
      retVal+=size.toString();
      retVal+="]";
      return retVal;
    }
    public int getLine(){
      return lineno;
    }
    public String typecheck(Map<String,String> typeMap){
      String ty=size.typecheck(typeMap);
      if(!ty.equals(INT)){
        logError(getLine(),"array declared with non-int size");
      }
      return ARR;
    }

    public String compile(LookupTable alloc){
      String reg=alloc.moveToTemp(size.compile(alloc));
      String vName=alloc.locToVar(reg);
      System.out.println("  #Line "+lineno+" arrayDec");
      System.out.println("  imull $4, "+reg);
      movl(reg,"(%esp)");
      alloc.flushTemps();
      System.out.println("  call malloc");
      String retReg=alloc.getRegister();
      movl("%eax",retReg);
      alloc.put(getTag(),retReg);
      return retReg;
    }
    public String getTag(){
      if(tag==null)
        tag="RVAL:ArrayDec"+System.nanoTime();
      return tag;
    }
  }

  public static class ClassDec implements Expr{
    final String type;
    final ActList args;
    final int lineno;
    public ClassDec(String type,ActList args){
      this.type=type;
      this.args=args;
      lineno=lex.getLine()+1;
    }

    public String typecheck(Map<String,String> typeMap){
      Class target=program.get(type);
      if(target==null){
        logError(getLine(),type+" is not a type.");
        System.out.println(this);
        return UNK;
      }
      MethodFeature func=target.getMethod("init");
      if(func==null)
      {
        if(args==null){
          return type;
        }
        else{
          logError(getLine(),"mismatch between init args and args supplied");
          return UNK;
        }
      }
      else
      {
        //TODO FIXME
        new MethodCall("self","init",args).typecheckArgs(typeMap,func);  
        return type;
      }
    }

    public String compile(LookupTable alloc){
      Class classname=program.get(type);
      MethodFeature mf=classname.getMethod("init");
      assert(mf!=null);
      //TODO FIXME
      MethodCall mc=new MethodCall("self","init",args);
      mf.setClass(type);
      mc.mTarget=mf;
      return mc.compile(alloc);
    }

    public int getLine(){
      return lineno;
    }

    public String getTag(){
      return "RVALClassDec"+type;
    }

  }

  public static class MethodCall implements Expr{
    final String id;
    final String method;
    final ActList args;
    MethodFeature mTarget;
    final int lineno;
    public MethodCall(String id,String method, ActList args){
      //System.err.println("id="+id);
      this.id=id;
      this.method=method;
      this.args=args;
      lineno=lex.getLine()+1;
    }

    public String toString(){
      return id+"."+method+"("+(args==null?"":args.toString())+")";
    }
    public int getLine(){
      return lineno;
    }
    public String typecheck(Map<String,String> typeMap){
      String classname=typeMap.get(id);
      MethodFeature func=checkBuiltIns();
      if(func==null){
        if(id.equals("self")||id.equals("")){
          //Special case #713
          classname=self.name;
        }
        else if(method.equals("init")&&
            program.keySet().contains(id))
        {
          //Special case #714
          classname=id;
        }
        if(classname==null){
          logError(getLine(),id+" is not a type.");
          System.out.println(this);
          return UNK;
        }
        Class target=program.get(classname);
        func=target.getMethod(method);
      } 
      if(func==null)
      {
        logError(getLine(),"class " + classname +
            " has no function " + method + ".");
        return UNK;
      }
      else
      {
        mTarget=func;
        typecheckArgs(typeMap,func);
        return func.type;
      }
    }

    public String compile(LookupTable alloc){
      System.out.println("  #Line "+lineno+" "+id+" "+method+"()");
      String retReg="%eax";
      alloc.flushTemps();
      if(mTarget==in_int||mTarget==in_string||
         mTarget==out_int||mTarget==out_string){
        String input=null;
        if(hasArgs()&&args.exprs.size()==1){
          input=args.exprs.get(0).compile(alloc);
        }
        retReg=compileBuiltIn(input,mTarget,alloc);
      }
      else if(mTarget==null){
        System.err.println(id+" "+method);
        assert(false);
      }
      else{
        String selfloc=alloc.varToLoc(id);
        String selfreg=alloc.loadIntoRegister(selfloc);
        pushArgs(selfreg,alloc);
        System.out.println("  call "+mTarget.getTag());
      }
      String reg=alloc.getRegister();
      movl(retReg,reg);
      alloc.put(getTag(),reg);
      return reg;
    }
  
    public boolean hasArgs(){
      return args!=null&&args.exprs!=null;
    }

    public void pushArgs(String self,LookupTable alloc){
      movl(self,"0(%esp)");
      int offset=4;
      if(hasArgs()){
        for(Expr act:args.exprs){
          String reg=act.compile(alloc);
          movl(reg,offset+"(%esp)");
          offset+=4;
        }
      }
    }

    public String compileBuiltIn(String reg,MethodFeature mf, LookupTable alloc){
      System.out.println("  #Line "+lineno+": calling "+mf.name);
      if(mf==in_int){
        movl("$_uncool_input","4(%esp)");
        movl("$.LC0", "(%esp)");
        System.out.println("  call scanf"); 
        movl("_uncool_input","%eax");
      }
      else if(mf==in_string){
        movl("$_uncool_input","4(%esp)");
        movl("$.LC2", "(%esp)");
        System.out.println("  call scanf"); 
        movl("_uncool_input","%eax");
      }
      else if(mf==out_int){
        movl(reg, "4(%esp)");
        movl("$.LC1","(%esp)");
        System.out.println("  call printf");
        movl(reg,"%eax"); 
      }
      else if(mf==out_string){
        movl(reg, "4(%esp)");
        movl("$.LC2", "(%esp)");
        System.out.println("  call printf");
        movl(reg,"%eax"); 
      }
      return "%eax";
    }
    public String getTag(){
      return "RVAL"+toString();
    }

    public MethodFeature checkBuiltIns(){
      if(id.equals("self")){
        switch(method){
          case "in_int":
            return in_int;
          case "in_string":
            return in_string;
          case "out_int":
            return out_int;
          case "out_string":
            return out_string;
        }
      } 
      return null;
    }

    public void typecheckArgs(Map<String,String> typeMap,MethodFeature func){
      ArrayList<String> argTypes=getTypeList(typeMap);
      ArrayList<Formal> actArgs=func.getDefs();
      if(actArgs.size()!=argTypes.size()){
        logError(lineno,method+" wants "+actArgs.size()+" args. "+argTypes.size()+" provided");
        if(actArgs.size()>argTypes.size()){
          actArgs.subList(argTypes.size(),actArgs.size()).clear();
        }
      }
      for(int x=0;x<actArgs.size();x++){
        Formal f=actArgs.get(x);
        String arg=argTypes.get(x);
        if(!f.type.equals(arg)){
          logError(getLine(),"Arg "+x+" in call to "+method+" should be "+f.type+" but is "+arg+".");
        }
      }
    }

    public ArrayList<String> getTypeList(Map<String,String> typeMap){
      if(args!=null)
        return args.getTypeList(typeMap);
      else
        return new ArrayList<String>();
      }
  }

  public static class Constant implements Expr{
    final String type;
    final String value;
    final int lineno;
    public Constant(String type){
      this.type=type;
      if(lex!=null)//Special case for builtins
        lineno=lex.getLine()+1;
      else
        lineno=0;
      value="0xDEADBEEF";
    }
    
    public Constant(String type, String value){
      this.type=type;
      if(lex!=null)//Special case for builtins
        lineno=lex.getLine()+1;
      else
        lineno=0;
      this.value=value;
    }

    public String toString(){
      return "const_"+value;
    }
    public int getLine(){
      return lineno;
    }
    public String typecheck(Map<String,String> typeMap){
      return type;
    }

    public String compile(LookupTable alloc){
      String reg=alloc.getRegister();
      String load=value;
      if(type.equals(STR)){
        String lab=getDataLabel();
        System.out.println("  .data");
        System.out.println("  "+lab+":");
        System.out.println("  .string "+load);
        System.out.println("  .align 4");
        System.out.println("  .text");
        load=lab;
      }
      boolean swit=true;
      if(swit){
        movl("$"+load,reg);
        alloc.put(getTag(),reg);
        return reg;
      }
      return "$"+load;
    }
    public String getTag(){
      return "RVAL"+toString();
    }
  }

  public static class OpExpr implements Expr{
    public enum OP{VOID,PLUS,MINUS,MULT,DIV,CMP,NOT,NE,GT,GE,LT,LE,EQ};
    public String[] ops={"isvoid","+","-","*","/","~","!",
              "!=",">",">=","<","<=","=="};
    public String[] instruct={"TODO","addl","subl","imull","idivl","negl","notl",
              "setne","setg","setge","setl","setle","sete"};
    final OP op;
    final Expr[] args;
    final int lineno;
    public OpExpr(OP op,Expr ... args){
      this.op=op;
      this.args=args;
      lineno=lex.getLine()+1;
    }

    public String toString(){
      if(args.length==1){
        return ops[op.ordinal()]+args[0].toString();
      }
      else{
        return args[0].toString()+ops[op.ordinal()]+args[1].toString();
      }
    }

    public int getLine(){
      return lineno;
    }
  
    public String typecheck(Map<String,String> typeMap){
      String t1=args[0].typecheck(typeMap);
      String t2=null;
      if(args.length>1){
        t2=args[1].typecheck(typeMap);
      }
      switch(op){
        case PLUS:
        case MINUS:
        case MULT:
        case DIV:
          if((!t1.equals(INT)&&!t1.equals(UNK))||
             (!t2.equals(INT)&&!t2.equals(UNK))){
            opError();
          }
          return INT;
        case GT:
        case GE:
        case LT:
        case LE:
          if((!t1.equals(INT)&&!t1.equals(UNK))||
             (!t2.equals(INT)&&!t2.equals(UNK))){
            opError();
          }
          return BOOL;
        case NE:
        case EQ:
          String common=Class.commonType(t1,t2);
          if(common.equals("N/A")){
            logError(getLine(),"t1!=t2 in comparison");
          }
          return BOOL;
        case CMP:
          if(!t1.equals(INT)&&!t1.equals(UNK)){
            opError();
          }
          return INT;
        case NOT:
          if(!t1.equals(BOOL)&&!t1.equals(UNK)){
            opError();
          }
          return BOOL;
        case VOID:
          return BOOL;
        default:
          logError(getLine(),"Unexpected opexpr");
          return UNK;
      }   
    }
    public void opError(){
      logError(getLine(),"Arg for "+ops[op.ordinal()]+" of wrong type.");
    }

    public String compile(LookupTable alloc){
      String t1=args[0].compile(alloc);
      String v1=alloc.locToVar(t1);
      assert(t1.equals(alloc.varToLoc(v1)));
      String t2=null;
      if(args.length>1){
        t2=args[1].compile(alloc);
        switch(op){
          case PLUS:
          case MULT:
          case NE:
          case EQ:
          if(alloc.isTemp(t2)&&!alloc.isTemp(t1)){
            String swap=t1;
            t1=t2;
            t2=swap;
          }
          default:
            //Nothing to be done
        }
        System.out.println("  # Line "+lineno+": "+v1+" "
                            +ops[op.ordinal()]+" "+alloc.locToVar(t2));
      }
      else
        System.out.println("  # Line "+lineno+": "+ops[op.ordinal()]+" "+v1);
      t1=alloc.moveToTemp(alloc.varToLoc(v1));
      //System.err.println("(t1,v1)=("+t1+","+v1+")");
      switch(op){
        case PLUS:
        case MINUS:
        case MULT:
        case DIV:
          System.out.println("  "+instruct[op.ordinal()]+" "+t2+","+t1);
          break;
        case GT:
        case GE:
        case LT:
        case LE:
        case NE:
        case EQ:
          System.out.println("  cmpl "+t2+","+t1);
          System.out.println("  "+instruct[op.ordinal()]+" %al");
          System.out.println("  movzbl %al, "+t1);
          break;
        case CMP:
        case NOT:
          System.out.println("  "+instruct[op.ordinal()]+" "+t1);
          break;
        case VOID:
          System.err.println("TODO "+ops[op.ordinal()]);
          throw new RuntimeException();
      }
    alloc.put(getTag(),t1);
    return t1;
    }
    public String getTag(){
      return "RVAL"+ops[op.ordinal()]+""+System.nanoTime();
    }
  }

  public static class ExprList{
    public ArrayList<Expr> exprs;
    public ExprList(ExprList es, Expr e){
      exprs=es.exprs;
      exprs.add(0,e);
    }

    public ExprList(Expr e){
      exprs=new ArrayList<Expr>();
      exprs.add(e);
    }

    public String toString(){
      String retVal="";
      for(int x=0;x<exprs.size()-1;x++){
        retVal+=exprs.get(x).toString()+";";
      }     
      if(exprs.size()>0){
        retVal+=exprs.get(exprs.size()-1).toString();
      }
      return retVal;
    }

    public String compile(LookupTable alloc){
      String reg=null;
      for(Expr e:exprs){
        reg=e.compile(alloc);
        alloc.clearTemps();
      }
      assert(reg!=null);
      return reg;
    }
  }

  public static class ActList{
    public ArrayList<Expr> exprs;
    public ActList(ActList o1, Expr o2){
      exprs=o1.exprs;
      exprs.add(0,o2);
    }

    public ActList(Expr o1){
      exprs=new ArrayList<Expr>();
      exprs.add(o1);
    }

    public String toString(){
      String retVal="";
      for(int x=0;x<exprs.size()-1;x++)
      {
        retVal+=exprs.get(x).toString()+",";
      }     
      if(exprs.size()>0)
      {
        retVal+=exprs.get(exprs.size()-1).toString();
      }
      return retVal;
    }

    public ArrayList<String> getTypeList(Map<String,String> typeMap){
      ArrayList<String> types=new ArrayList<String>();
      for(Expr e:exprs)
      {
        String type=e.typecheck(typeMap);
        types.add(type);
      }
      return types;
    }
  }
