//
// Diese Datei wurde mit der JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.2.8-b130911.1802 generiert 
// Siehe <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Änderungen an dieser Datei gehen bei einer Neukompilierung des Quellschemas verloren. 
// Generiert: 2015.11.23 um 05:38:11 PM CET 
//


package de.innovationgate.wgpublisher.test.log;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the de.innovationgate.wgpublisher.test.log package. 
 * <p>An ObjectFactory allows you to programatically 
 * construct new instances of the Java representation 
 * for XML content. The Java representation of XML 
 * content can consist of schema derived interfaces 
 * and classes representing the binding of schema 
 * type definitions, element declarations and model 
 * groups.  Factory methods for each of these are 
 * provided in this class.
 * 
 */
@XmlRegistry
public class ObjectFactory {

    private final static QName _Testsuite_QNAME = new QName("", "testsuite");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: de.innovationgate.wgpublisher.test.log
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link Testsuites }
     * 
     */
    public Testsuites createTestsuites() {
        return new Testsuites();
    }

    /**
     * Create an instance of {@link de.innovationgate.wgpublisher.test.log.Testsuite }
     * 
     */
    public de.innovationgate.wgpublisher.test.log.Testsuite createTestsuite() {
        return new de.innovationgate.wgpublisher.test.log.Testsuite();
    }

    /**
     * Create an instance of {@link de.innovationgate.wgpublisher.test.log.Testsuite.Testcase }
     * 
     */
    public de.innovationgate.wgpublisher.test.log.Testsuite.Testcase createTestsuiteTestcase() {
        return new de.innovationgate.wgpublisher.test.log.Testsuite.Testcase();
    }

    /**
     * Create an instance of {@link de.innovationgate.wgpublisher.test.log.Testsuite.Properties }
     * 
     */
    public de.innovationgate.wgpublisher.test.log.Testsuite.Properties createTestsuiteProperties() {
        return new de.innovationgate.wgpublisher.test.log.Testsuite.Properties();
    }

    /**
     * Create an instance of {@link Testsuites.Testsuite }
     * 
     */
    public Testsuites.Testsuite createTestsuitesTestsuite() {
        return new Testsuites.Testsuite();
    }

    /**
     * Create an instance of {@link de.innovationgate.wgpublisher.test.log.Testsuite.Testcase.Error }
     * 
     */
    public de.innovationgate.wgpublisher.test.log.Testsuite.Testcase.Error createTestsuiteTestcaseError() {
        return new de.innovationgate.wgpublisher.test.log.Testsuite.Testcase.Error();
    }

    /**
     * Create an instance of {@link de.innovationgate.wgpublisher.test.log.Testsuite.Testcase.Failure }
     * 
     */
    public de.innovationgate.wgpublisher.test.log.Testsuite.Testcase.Failure createTestsuiteTestcaseFailure() {
        return new de.innovationgate.wgpublisher.test.log.Testsuite.Testcase.Failure();
    }

    /**
     * Create an instance of {@link de.innovationgate.wgpublisher.test.log.Testsuite.Properties.Property }
     * 
     */
    public de.innovationgate.wgpublisher.test.log.Testsuite.Properties.Property createTestsuitePropertiesProperty() {
        return new de.innovationgate.wgpublisher.test.log.Testsuite.Properties.Property();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link de.innovationgate.wgpublisher.test.log.Testsuite }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "", name = "testsuite")
    public JAXBElement<de.innovationgate.wgpublisher.test.log.Testsuite> createTestsuite(de.innovationgate.wgpublisher.test.log.Testsuite value) {
        return new JAXBElement<de.innovationgate.wgpublisher.test.log.Testsuite>(_Testsuite_QNAME, de.innovationgate.wgpublisher.test.log.Testsuite.class, null, value);
    }

}
