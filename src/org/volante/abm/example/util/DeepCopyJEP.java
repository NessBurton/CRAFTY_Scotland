/**
 * This file is part of
 * 
 * CRAFTY - Competition for Resources between Agent Functional TYpes
 *
 * Copyright (C) 2016 School of GeoScience, University of Edinburgh, Edinburgh, UK
 * 
 * CRAFTY is free software: You can redistribute it and/or modify it under the
 * terms of the GNU General Public License as published by the Free Software 
 * Foundation, either version 3 of the License, or (at your option) any later
 * version.
 *  
 * CRAFTY is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty
 * of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * School of Geoscience, University of Edinburgh, Edinburgh, UK
 * 
 * Created by Sascha Holzhauer on 5 Jan 2016
 */
package org.volante.abm.example.util;


/*****************************************************************************

 JEP - Java Expression Parser
 JEP is a Java package for parsing and evaluating mathematical
 expressions. It currently supports user defined variables,
 constant, and functions. A number of common mathematical
 functions and constants are included.

 Author: Nathan Funk
 Copyright (C) 2000 Nathan Funk

 JEP is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2 of the License, or
 (at your option) any later version.

 JEP is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with JEP; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************/

import java.io.Reader;
import java.io.StringReader;
import java.util.Vector;

import org.lsmp.djep.xjep.XJep;
import org.nfunk.jep.EvaluatorVisitor;
import org.nfunk.jep.FunctionTable;
import org.nfunk.jep.JEP;
import org.nfunk.jep.Node;
import org.nfunk.jep.OperatorSet;
import org.nfunk.jep.ParseException;
import org.nfunk.jep.Parser;
import org.nfunk.jep.ParserDumpVisitor;
import org.nfunk.jep.ParserVisitor;
import org.nfunk.jep.SymbolTable;
import org.nfunk.jep.Variable;
import org.nfunk.jep.VariableFactory;
import org.nfunk.jep.function.Abs;
import org.nfunk.jep.function.ArcCosine;
import org.nfunk.jep.function.ArcCosineH;
import org.nfunk.jep.function.ArcSine;
import org.nfunk.jep.function.ArcSineH;
import org.nfunk.jep.function.ArcTanH;
import org.nfunk.jep.function.ArcTangent;
import org.nfunk.jep.function.ArcTangent2;
import org.nfunk.jep.function.Arg;
import org.nfunk.jep.function.Binomial;
import org.nfunk.jep.function.Ceil;
import org.nfunk.jep.function.ComplexPFMC;
import org.nfunk.jep.function.Conjugate;
import org.nfunk.jep.function.Cosine;
import org.nfunk.jep.function.CosineH;
import org.nfunk.jep.function.Exp;
import org.nfunk.jep.function.Floor;
import org.nfunk.jep.function.If;
import org.nfunk.jep.function.Imaginary;
import org.nfunk.jep.function.Logarithm;
import org.nfunk.jep.function.Modulus;
import org.nfunk.jep.function.NaturalLogarithm;
import org.nfunk.jep.function.Polar;
import org.nfunk.jep.function.PostfixMathCommandI;
import org.nfunk.jep.function.Power;
import org.nfunk.jep.function.Real;
import org.nfunk.jep.function.Round;
import org.nfunk.jep.function.Sine;
import org.nfunk.jep.function.SineH;
import org.nfunk.jep.function.SquareRoot;
import org.nfunk.jep.function.Str;
import org.nfunk.jep.function.Sum;
import org.nfunk.jep.function.TanH;
import org.nfunk.jep.function.Tangent;
import org.nfunk.jep.type.Complex;
import org.nfunk.jep.type.DoubleNumberFactory;
import org.nfunk.jep.type.NumberFactory;

/**
 * The JEP class is the main interface with which the user should interact. It contains all neccessary methods to parse
 * and evaluate expressions.
 * <p>
 * The most important methods are parseExpression(String), for parsing the mathematical expression, and getValue() for
 * obtaining the value of the expression.
 * <p>
 * Visit <a href="http://jep.sourceforge.net/">http://jep.sourceforge.net/</a> for the newest version of JEP, and
 * complete documentation.
 * 
 * @author Nathan Funk
 * @author Sascha Holzhauer
 * 
 */
public class DeepCopyJEP extends JEP {

	/** Debug flag for extra command line output */
	private static final boolean debug = false;

	/** Traverse option */
	private boolean traverse;

	/** The parser object */
	private Parser parser;

	/** Node at the top of the parse tree */
	private Node deepCopyTopNode;

	/** Evaluator */
	private EvaluatorVisitor ev;

	/** Number factory */
	private NumberFactory numberFactory;

	/**
	 * Creates a new JEP instance with the default settings.
	 * <p>
	 * Traverse = false<br>
	 * Allow undeclared variables = false<br>
	 * Implicit multiplication = false<br>
	 * Number Factory = DoubleNumberFactory
	 */
	public DeepCopyJEP() {
		deepCopyTopNode = null;
		traverse = false;
		numberFactory = new DoubleNumberFactory();
		initSymTab();
		initFunTab();
		ev = new EvaluatorVisitor();
		parser = new Parser(new StringReader(""));

		// Ensure errors are reported for the initial expression
		// e.g. No expression entered
		// >> out-commented because clones JEPs do not parse before usage.
		// parseExpression("");
	}

	/**
	 * Creates a new JEP instance with custom settings. If the numberFactory_in is null, the default number factory is
	 * used.
	 * 
	 * @param traverse_in
	 *        The traverse option.
	 * @param allowUndeclared_in
	 *        The "allow undeclared variables" option.
	 * @param implicitMul_in
	 *        The implicit multiplication option.
	 * @param numberFactory_in
	 *        The number factory to be used.
	 */
	public DeepCopyJEP(boolean traverse_in, boolean allowUndeclared_in, boolean implicitMul_in,
			NumberFactory numberFactory_in) {
		deepCopyTopNode = null;
		traverse = traverse_in;
		allowUndeclared = allowUndeclared_in;
		implicitMul = implicitMul_in;
		if (numberFactory_in == null) {
			numberFactory = new DoubleNumberFactory();
		} else {
			numberFactory = numberFactory_in;
		}
		initSymTab();
		initFunTab();
		errorList = new Vector();
		ev = new EvaluatorVisitor();
		parser = new Parser(new StringReader(""));

		// Ensure errors are reported for the initial expression
		// e.g. No expression entered
		parseExpression("");
	}

	/**
	 * Creates a new SymbolTable object as symTab.
	 */
	public void initSymTab() {
		// Init SymbolTable
		symTab = new SymbolTable(new VariableFactory());
	}

	/**
	 * Creates a new FunctionTable object as funTab.
	 */
	public void initFunTab() {
		// Init FunctionTable
		funTab = new FunctionTable();
	}

	/**
	 * Adds the standard functions to the parser. If this function is not called before parsing an expression, functions
	 * such as sin() or cos() would produce an "Unrecognized function..." error. In most cases, this method should be
	 * called immediately after the JEP object is created.
	 * 
	 * @since 2.3.0 alpha added if and exp functions
	 * @since 2.3.0 beta 1 added str function
	 */
	public void addStandardFunctions() {
		// add functions to Function Table
		funTab.put("sin", new Sine());
		funTab.put("cos", new Cosine());
		funTab.put("tan", new Tangent());
		funTab.put("asin", new ArcSine());
		funTab.put("acos", new ArcCosine());
		funTab.put("atan", new ArcTangent());
		funTab.put("atan2", new ArcTangent2());

		funTab.put("sinh", new SineH());
		funTab.put("cosh", new CosineH());
		funTab.put("tanh", new TanH());
		funTab.put("asinh", new ArcSineH());
		funTab.put("acosh", new ArcCosineH());
		funTab.put("atanh", new ArcTanH());

		funTab.put("log", new Logarithm());
		funTab.put("ln", new NaturalLogarithm());
		funTab.put("exp", new Exp());
		funTab.put("pow", new Power());

		funTab.put("sqrt", new SquareRoot());
		funTab.put("abs", new Abs());
		funTab.put("mod", new Modulus());
		funTab.put("sum", new Sum());

		funTab.put("rand", new org.nfunk.jep.function.Random());

		// rjm additions
		funTab.put("if", new If());
		funTab.put("str", new Str());

		// rjm 13/2/05
		funTab.put("binom", new Binomial());

		// rjm 26/1/07
		funTab.put("round", new Round());
		funTab.put("floor", new Floor());
		funTab.put("ceil", new Ceil());
	}

	/**
	 * Adds the constants pi and e to the parser. As addStandardFunctions(), this method should be called immediately
	 * after the JEP object is created.
	 */
	public void addStandardConstants() {
		// add constants to Symbol Table
		symTab.addConstant("pi", new Double(Math.PI));
		symTab.addConstant("e", new Double(Math.E));
	}

	/**
	 * Call this function if you want to parse expressions which involve complex numbers. This method specifies "i" as
	 * the imaginary unit (0,1). Two functions re() and im() are also added for extracting the real or imaginary
	 * components of a complex number respectively.
	 * <p>
	 * 
	 * @since 2.3.0 alpha The functions cmod and arg are added to get the modulus and argument.
	 * @since 2.3.0 beta 1 The functions complex and polar to convert x,y and r,theta to Complex.
	 * @since Feb 05 added complex conjugate conj.
	 */
	public void addComplex() {
		// add constants to Symbol Table
		symTab.addConstant("i", new Complex(0, 1));
		funTab.put("re", new Real());
		funTab.put("im", new Imaginary());
		funTab.put("arg", new Arg());
		funTab.put("cmod", new Abs());
		funTab.put("complex", new ComplexPFMC());
		funTab.put("polar", new Polar());
		funTab.put("conj", new Conjugate());
	}

	/**
	 * Adds a new function to the parser. This must be done before parsing an expression so the parser is aware that the
	 * new function may be contained in the expression.
	 * 
	 * @param functionName
	 *        The name of the function
	 * @param function
	 *        The function object that is used for evaluating the function
	 */
	public void addFunction(String functionName, PostfixMathCommandI function) {
		funTab.put(functionName, function);
	}

	/**
	 * Adds a constant. This is a variable whose value cannot be changed.
	 * 
	 * @since 2.3.0 beta 1
	 */
	public void addConstant(String name, Object value) {
		symTab.addConstant(name, value);
	}

	/**
	 * Adds a new variable to the parser, or updates the value of an existing variable. This must be done before parsing
	 * an expression so the parser is aware that the new variable may be contained in the expression.
	 * 
	 * @param name
	 *        Name of the variable to be added
	 * @param value
	 *        Initial value or new value for the variable
	 * @return Double object of the variable
	 */
	public Double addVariable(String name, double value) {
		Double object = new Double(value);
		symTab.makeVarIfNeeded(name, object);
		return object;
	}


		/**
	 * Adds a new complex variable to the parser, or updates the value of an existing variable. This must be done before
	 * parsing an expression so the parser is aware that the new variable may be contained in the expression.
	 * 
	 * @param name
	 *        Name of the variable to be added
	 * @param re
	 *        Initial real value or new real value for the variable
	 * @param im
	 *        Initial imaginary value or new imaginary value for the variable
	 * @return Complex object of the variable
	 */
	public Complex addVariable(String name, double re, double im) {
		Complex object = new Complex(re, im);
		symTab.makeVarIfNeeded(name, object);
		return object;
	}

	/**
	 * Adds a new variable to the parser as an object, or updates the value of an existing variable. This must be done
	 * before parsing an expression so the parser is aware that the new variable may be contained in the expression.
	 * 
	 * @param name
	 *        Name of the variable to be added
	 * @param object
	 *        Initial value or new value for the variable
	 */
	public void addVariable(String name, Object object) {
		symTab.makeVarIfNeeded(name, object);
	}

	/**
	 * Removes a variable from the parser. For example after calling addStandardConstants(), removeVariable("e") might
	 * be called to remove the euler constant from the set of variables.
	 * 
	 * @return The value of the variable if it was added earlier. If the variable is not in the table of variables,
	 *         <code>null</code> is returned.
	 */
	public Object removeVariable(String name) {
		return symTab.remove(name);
	}

	/**
	 * Returns the value of the variable with given name.
	 * 
	 * @param name
	 *        name of the variable.
	 * @return the current value of the variable.
	 * @since 2.3.0 alpha
	 */
	public Object getVarValue(String name) {
		return symTab.getVar(name).getValue();
	}

	/**
	 * Sets the value of a variable. The variable must exist before hand.
	 * 
	 * @param name
	 *        name of the variable.
	 * @param val
	 *        the initial value of the variable.
	 * @throws NullPointerException
	 *         if the variable has not been previously created with {@link #addVariable(String,Object)} first.
	 * @since 2.3.0 alpha
	 * @since April 05 - throws an exception if variable unset.
	 */
	public void setVarValue(String name, Object val) {
		symTab.setVarValue(name, val);
	}

	/**
	 * Gets the object representing the variable with a given name.
	 * 
	 * @param name
	 *        the name of the variable to find.
	 * @return the Variable object or null if name not found.
	 * @since 2.3.0 alpha
	 */
	public Variable getVar(String name) {
		return symTab.getVar(name);
	}

	/**
	 * Removes a function from the parser.
	 * 
	 * @return If the function was added earlier, the function class instance is returned. If the function was not
	 *         present, <code>null</code> is returned.
	 */
	public Object removeFunction(String name) {
		return funTab.remove(name);
	}

	/**
	 * Sets the value of the traverse option. setTraverse is useful for debugging purposes. When traverse is set to
	 * true, the parse-tree will be dumped to the standard output device.
	 * <p>
	 * The default value is false.
	 * 
	 * @param value
	 *        The boolean traversal option.
	 */
	public void setTraverse(boolean value) {
		traverse = value;
	}

	/**
	 * Returns the value of the traverse option.
	 * 
	 * @return True if the traverse option is enabled. False otherwise.
	 */
	public boolean getTraverse() {
		return traverse;
	}

	/**
	 * Sets the value of the implicit multiplication option. If this option is set to true before parsing, implicit
	 * multiplication will be allowed. That means that an expression such as
	 * 
	 * <pre>
	 * &quot;1 2&quot;
	 * </pre>
	 * 
	 * is valid and is interpreted as
	 * 
	 * <pre>
	 * &quot;1*2&quot;
	 * </pre>
	 * 
	 * .
	 * <p>
	 * The default value is false.
	 * 
	 * @param value
	 *        The boolean implicit multiplication option.
	 */
	public void setImplicitMul(boolean value) {
		implicitMul = value;
	}

	/**
	 * Returns the value of the implicit multiplication option.
	 * 
	 * @return True if the implicit multiplication option is enabled. False otherwise.
	 */
	public boolean getImplicitMul() {
		return implicitMul;
	}

	/**
	 * Sets the value for the undeclared variables option. If this option is set to true, expressions containing
	 * variables that were not previously added to JEP will not produce an "Unrecognized Symbol" error. The new
	 * variables will automatically be added while parsing, and initialized to 0.
	 * <p>
	 * If this option is set to false, variables that were not previously added to JEP will produce an error while
	 * parsing.
	 * <p>
	 * The default value is false.
	 * 
	 * @param value
	 *        The boolean option for allowing undeclared variables.
	 */
	public void setAllowUndeclared(boolean value) {
		allowUndeclared = value;
	}

	/**
	 * Returns the value of the allowUndeclared option.
	 * 
	 * @return True if the allowUndeclared option is enabled. False otherwise.
	 */
	public boolean getAllowUndeclared() {
		return allowUndeclared;
	}

	/**
	 * Sets whether assignment equations like <tt>y=x+1</tt> are allowed.
	 * 
	 * @since 2.3.0 alpha
	 */
	public void setAllowAssignment(boolean value) {
		allowAssignment = value;
	}

	/**
	 * Whether assignment equation <tt>y=x+1</tt> equations are allowed.
	 * 
	 * @since 2.3.0 alpha
	 */
	public boolean getAllowAssignment() {
		return allowAssignment;
	}

	/**
	 * Parses the expression. If there are errors in the expression, they are added to the <code>errorList</code>
	 * member. Errors can be obtained through <code>getErrorInfo()</code>.
	 * 
	 * @param expression_in
	 *        The input expression string
	 * @return the top node of the expression tree if the parse was successful, <code>null</code> otherwise
	 */
	public Node parseExpression(String expression_in) {
		Reader reader = new StringReader(expression_in);

		try {
			// try parsing
			errorList.removeAllElements();
			deepCopyTopNode = parser.parseStream(reader, this);

			// if there is an error in the list, the parse failed
			// so set topNode to null
			if (hasError())
				deepCopyTopNode = null;
		} catch (Throwable e) {
			// an exception was thrown, so there is no parse tree
			deepCopyTopNode = null;

			// check the type of error
			if (e instanceof ParseException) {
				// the ParseException object contains additional error
				// information
				errorList.addElement(((ParseException) e).getMessage());
				// getErrorInfo());
			} else {
				// if the exception was not a ParseException, it was most
				// likely a syntax error
				if (debug) {
					System.out.println(e.getMessage());
					e.printStackTrace();
				}
				errorList.addElement("Syntax error");
			}
		}

		// If traversing is enabled, print a dump of the tree to
		// standard output
		if (traverse && !hasError()) {
			ParserVisitor v = new ParserDumpVisitor();
			try {
				deepCopyTopNode.jjtAccept(v, null);
			} catch (ParseException e) {
				errorList.addElement(e.getMessage());
			}
		}

		return deepCopyTopNode;
	}

	/**
	 * Parses an expression. Returns a object of type Node, does not catch errors. Does not set the topNode variable of
	 * the JEP instance. This method should generally be used with the {@link #evaluate evaluate} method rather than
	 * getValueAsObject.
	 * 
	 * @param expression
	 *        represented as a string.
	 * @return The top node of a tree representing the parsed expression.
	 * @throws ParseException
	 * @since 2.3.0 alpha
	 * @since 2.3.0 beta - will raise exception if errorList non empty
	 */
	public Node parse(String expression) throws ParseException {
		java.io.StringReader sr = new java.io.StringReader(expression);
		errorList.removeAllElements();
		Node node = parser.parseStream(sr, this);
		if (this.hasError())
			throw new ParseException(getErrorInfo());
		return node;
	}

	/**
	 * Evaluate an expression. This method evaluates the argument rather than the topNode of the JEP instance. It should
	 * be used in conjunction with {@link #parse parse} rather than {@link #parseExpression parseExpression}.
	 * 
	 * @param node
	 *        the top node of the tree representing the expression.
	 * @return The value of the expression
	 * @throws ParseException
	 *         if for some reason the expression could not be evaluated
	 * @throws RuntimeException
	 *         could potentially be thrown.
	 * @since 2.3.0 alpha
	 */
	public Object evaluate(Node node) throws ParseException {
		return ev.getValue(node, this.symTab);
	}

	/**
	 * Evaluates and returns the value of the expression as a double number.
	 * 
	 * @return The calculated value of the expression as a double number. If the type of the value does not implement
	 *         the Number interface (e.g. Complex), NaN is returned. If an error occurs during evaluation, NaN is
	 *         returned and hasError() will return true.
	 * 
	 * @see #getComplexValue()
	 */
	public double getValue() {
		Object value = getValueAsObject();

		if (value == null)
			return Double.NaN;

		if (value instanceof Complex) {
			Complex c = (Complex) value;
			if (c.im() != 0.0)
				return Double.NaN;
			return c.re();
		}
		if (value != null && value instanceof Number) {
			return ((Number) value).doubleValue();
		}

		return Double.NaN;
	}


		/**
	 * Evaluates and returns the value of the expression as a complex number.
	 * 
	 * @return The calculated value of the expression as a complex number if no errors occur. Returns null otherwise.
	 */
	public Complex getComplexValue() {
		Object value = getValueAsObject();

		if (value == null) {
			return null;
		} else if (value instanceof Complex) {
			return (Complex) value;
		} else if (value instanceof Number) {
			return new Complex(((Number) value).doubleValue(), 0);
		} else {
			return null;
		}
	}


	/**
	 * Evaluates and returns the value of the expression as an object. The EvaluatorVisitor member ev is used to do the
	 * evaluation procedure. This method is useful when the type of the value is unknown, or not important.
	 * 
	 * @return The calculated value of the expression if no errors occur. Returns null otherwise.
	 */
	public Object getValueAsObject() {
		Object result;

		// fail by returning null if no topNode is available
		if (deepCopyTopNode == null)
			return null;

		// evaluate the expression
		try {
			result = ev.getValue(deepCopyTopNode, symTab);
		} catch (ParseException e) {
			if (debug)
				System.out.println(e);
			errorList.addElement("Error during evaluation: " + e.getMessage());
			return null;
		} catch (RuntimeException e) {
			if (debug)
				System.out.println(e);
			errorList.addElement(e.getClass().getName() + ": " + e.getMessage());
			return null;
		}
		return result;
	}

	/**
	 * Returns true if an error occurred during the most recent action (parsing or evaluation).
	 * 
	 * @return Returns <code>true</code> if an error occurred during the most recent action (parsing or evaluation).
	 */
	public boolean hasError() {
		return !errorList.isEmpty();
	}

	/**
	 * Reports information on the errors that occurred during the most recent action.
	 * 
	 * @return A string containing information on the errors, each separated by a newline character; null if no error
	 *         has occurred
	 */
	public String getErrorInfo() {
		if (hasError()) {
			String str = "";

			// iterate through all errors and add them to the return string
			for (int i = 0; i < errorList.size(); i++) {
				str += errorList.elementAt(i) + "\n";
			}
			return str;
		}
		return null;
	}

	/**
	 * Returns the top node of the expression tree. Because all nodes are pointed to either directly or indirectly, the
	 * entire expression tree can be accessed through this node. It may be used to manipulate the expression, and
	 * subsequently evaluate it manually.
	 * 
	 * @return The top node of the expression tree
	 */
	public Node getTopNode() {
		return deepCopyTopNode;
	}

	/**
	 * Returns the symbol table (the list of all variables that the parser recognizes).
	 * 
	 * @return The symbol table
	 */
	public SymbolTable getSymbolTable() {
		return symTab;
	}

	/**
	 * Returns the function table (the list of all functions that the parser recognizes).
	 * 
	 * @return The function table
	 */
	public FunctionTable getFunctionTable() {
		return funTab;
	}

	/**
	 * Returns the EvaluatorVisitor
	 * 
	 * @return the EvaluatorVisitor.
	 */
	public EvaluatorVisitor getEvaluatorVisitor() {
		return ev;
	}

	/**
	 * Returns the number factory.
	 * 
	 * @return the NumberFactory used by this JEP instance.
	 */
	public NumberFactory getNumberFactory() {
		return numberFactory;
	}

	/**
	 * Returns the operator set.
	 * 
	 * @return the OperatorSet used by this JEP instance.
	 * @since 2.3.0 alpha
	 */
	public OperatorSet getOperatorSet() {
		return opSet;
	}

	/**
	 * Returns the parse object.
	 * 
	 * @return the Parse used by this JEP.
	 * @since 2.3.0 beta 1
	 */
	public Parser getParser() {
		return parser;
	}

	public DeepCopyJEP deepCopyJepParser() throws ParseException {

		DeepCopyJEP jepCopy = new DeepCopyJEP();

		// copy function table
		jepCopy.funTab = (FunctionTable) this.funTab.clone();

		// copy symbol table
		jepCopy.symTab = (SymbolTable) this.symTab.clone();
		
		jepCopy.parser = this.parser;

		// deep copy top node
		// http://www.cin.ufpe.br/~gfsv/gfsv/workspace/
		XJep xjep = new XJep(this);
		jepCopy.deepCopyTopNode = xjep.deepCopy(this.deepCopyTopNode);

		// copy settings
		jepCopy.setImplicitMul(this.implicitMul);
		jepCopy.allowUndeclared = this.allowUndeclared;
		
		return jepCopy;
	}
}

