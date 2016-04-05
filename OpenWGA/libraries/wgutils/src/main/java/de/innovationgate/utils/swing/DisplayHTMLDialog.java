/*******************************************************************************
 * Copyright 2009, 2010 Innovation Gate GmbH
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/
package de.innovationgate.utils.swing;
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JEditorPane;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.border.BevelBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.text.html.HTMLEditorKit;


/**
 * A small modal dialog to display informative HTML code.
 * The dialog consists of a central text area showing HTML code.
 * It may show a single "close" button, or two buttons labeled "OK" and "Cancel".
 * It additionally may show some "header text" displayed above the text area.
 *
 */
public class DisplayHTMLDialog extends JDialog {

    private static final long serialVersionUID = 1L;

    private JPanel jContentPane = null;

    private JPanel panButtons = null;

    private JButton btoClose = null;

    private JScrollPane scrTemplateDescription = null;

    private JEditorPane txtTemplateDescription = null;

    private JLabel lblHeader = null;

    private boolean _okPressed = false;

    private JButton btoOK;

    /**
     * Creates the dialog object
     * @param owner The owner frame of the dialog
     */
    public DisplayHTMLDialog(Frame owner) {
        super(owner);
        initialize();
        getRootPane().setDefaultButton(getBtoCancel());
    }

    /**
     * This method initializes this
     * 
     * @return void
     */
    private void initialize() {
        this.setSize(608, 367);
        this.setModal(false);
        this.setTitle("Dialog title");
        this.setContentPane(getJContentPane());
    }

    /**
     * This method initializes jContentPane
     * 
     * @return javax.swing.JPanel
     */
    private JPanel getJContentPane() {
        if (jContentPane == null) {
            jContentPane = new JPanel();
            jContentPane.setBorder(new EmptyBorder(5,5,5,5));
            BorderLayout borderLayout = new BorderLayout();
            borderLayout.setHgap(5);
            borderLayout.setVgap(5);
            jContentPane.setLayout(borderLayout);
            jContentPane.add(getLblHeader(), BorderLayout.NORTH);
            jContentPane.add(getScrTemplateDescription(), BorderLayout.CENTER);
            jContentPane.add(getPanButtons(), BorderLayout.SOUTH);
        }
        return jContentPane;
    }

    private JLabel getLblHeader() {
        if (lblHeader == null) {
            lblHeader = new JLabel();
            lblHeader.setVisible(false);
        }
        return lblHeader;
    }

    /**
     * This method initializes panButtons	
     * 	
     * @return javax.swing.JPanel	
     */
    private JPanel getPanButtons() {
        if (panButtons == null) {
            GridBagConstraints gridBagConstraints6 = new GridBagConstraints();
            gridBagConstraints6.insets = new Insets(5, 5, 5, 5);
            GridBagConstraints gridBagConstraints7 = new GridBagConstraints();
            gridBagConstraints7.insets = new Insets(5, 5, 5, 5);
            panButtons = new JPanel();
            panButtons.setLayout(new GridBagLayout());
            panButtons.add(getBtoOK(), gridBagConstraints6);
            panButtons.add(getBtoCancel(), gridBagConstraints7);
        }
        return panButtons;
    }

    /**
     * This method initializes btoCancel	
     * 	
     * @return javax.swing.JButton	
     */
    private JButton getBtoCancel() {
        if (btoClose == null) {
            btoClose = new JButton();
            btoClose.setText("Close");
            btoClose.addActionListener(new ActionListener() {

                public void actionPerformed(ActionEvent e) {
                    dispose();
                }
                
            });
        }
        return btoClose;
    }
    
    /**
     * This method initializes btoOK    
     *  
     * @return javax.swing.JButton  
     */
    private JButton getBtoOK() {
        if (btoOK == null) {
            btoOK = new JButton();
            btoOK.setText("OK");
            btoOK.setVisible(false);
            btoOK.addActionListener(new ActionListener() {

                public void actionPerformed(ActionEvent e) {
                    _okPressed = true;
                    dispose();
                }
                
            });
        }
        return btoOK;
    }
    
    /**
     * This method initializes scrTemplateDescription	
     * 	
     * @return javax.swing.JScrollPane	
     */
    private JScrollPane getScrTemplateDescription() {
        if (scrTemplateDescription == null) {
            scrTemplateDescription = new JScrollPane();
            scrTemplateDescription.setBorder(BorderFactory.createBevelBorder(BevelBorder.LOWERED));
            scrTemplateDescription.setViewportView(getTxtTemplateDescription());
        }
        return scrTemplateDescription;
    }

    /**
     * This method initializes txtTemplateDescription	
     * 	
     * @return javax.swing.JEditorPane	
     */
    private JEditorPane getTxtTemplateDescription() {
        if (txtTemplateDescription == null) {
            txtTemplateDescription = new JEditorPane();
            txtTemplateDescription.setEditable(false);
            txtTemplateDescription.setEditorKit(new HTMLEditorKit());
            txtTemplateDescription.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
            
        }
        return txtTemplateDescription;
    }
    
    /**
     * Sets the HTML code to display
     */
    public void setHTML(String html) {
        getTxtTemplateDescription().setDocument(getTxtTemplateDescription().getEditorKit().createDefaultDocument());
        getTxtTemplateDescription().setText(html);
        getTxtTemplateDescription().setCaretPosition(0);
        
    }
    
    /**
     * Set the header text
     */
    public void setHeader(String header) {
        getLblHeader().setText(header);
        getLblHeader().setVisible(true);
    }
    
    /**
     * Display the dialog
     * This method will return when the dialog is closed.
     * @param relativeTo A component that the dialog should be placed relative to
     */
    public void display(Component relativeTo) {
        if (relativeTo != null) {
            setLocationRelativeTo(relativeTo);
        }
        setVisible(true);
        getTxtTemplateDescription().scrollToReference("top");
    }
    
    /**
     * Display the dialog
     * This method will return when the dialog is closed.
     */
    public void display() {
        display(null);
    }

    /**
     * Returns if the OK button was pressed after the dialog is closed
     */
    public boolean isOkPressed() {
        return _okPressed;
    }
    
    /**
     * Sets if the buttons should be "OK" and "Cancel" instead of one single "Close"
     */
    public void setDisplayOkButton(boolean display) {
        getBtoOK().setVisible(display);
        getBtoCancel().setText(display ? "Cancel" : "Close");
    }
    
    /**
     * Returns if the buttons should be "OK" and "Cancel" instead of one single "Close"
     */
    public boolean isDisplayOkButton() {
        return getBtoOK().isVisible();
    }
    

    

}  //  @jve:decl-index=0:visual-constraint="10,10"
