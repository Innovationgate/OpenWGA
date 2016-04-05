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
package de.innovationgate.utils;

/**
*  Instant Java Servlets
*  Figure 12-26
*
*  Copyright (c) 2000 by Phillip Hanna
*  All rights reserved.
*
*  You may study, use, modify, and distribute this
*  software for any purpose provided that this
*  copyright notice appears in all copies.
*
*  This software is provided without warranty
*  either expressed or implied.
*/

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.Vector;

import de.innovationgate.wga.modules.options.PasswordEncodingException;
import de.innovationgate.wga.modules.options.PasswordOptionEncoder;

/**
 * General purpose class to convert bytes to Base64 encoded strings or vice versa.
 * Is able to use the "traditional" encoding pattern (using + and / characters
 * that are not safe in URLs) and the "web-safe" encoding (using - and _ instead).
 * 
 */
public class Base64 implements PasswordOptionEncoder {

   /**
   * The translation table
   */
   public static final String base64Table =
      "ABCDEFGHIJKLMNOPQRSTUVWXYZ" +
      "abcdefghijklmnopqrstuvwxyz" +
      "0123456789" +
      "+/";

   /**
    * WebBase64Table view RFC3548 for details
    */
   public static final String base64TableWeb =
       "ABCDEFGHIJKLMNOPQRSTUVWXYZ" +
       "abcdefghijklmnopqrstuvwxyz" +
       "0123456789" +
       "-_";
   
   /**
   * Decodes the given string buffer
   * This method version uses the "traditional" set of base64 characters that are not all URL-safe,
   * containing alphanumeric characters plus characters "/" and "+". 
   * Be sure that the base64 string was created with the same character set when using it.
   * @param buffer the string to be decoded
   * @return the decoded byte array
   * @exception java.io.IOException if the buffer
   * contains invalid data
   */
   public static byte[] decode(String buffer)
      throws IOException
   {
      if ((buffer.length() % 4) != 0)
         throw new IOException
         ("Buffer length not a multiple of four");

      int state = 0;
      int ch = 0;
      Vector<Integer> result = new Vector<Integer>();
      for (int i = 0; i < buffer.length(); i++) {
         int b = buffer.charAt(i);
         int p = base64Table.indexOf(b);
         if (p == -1)
            if (b == '=')
               p = 0;
            else
               throw new IOException
               ("Invalid character in input");

         switch (state) {

            case 0:
               ch = (p << 2) & 0xFC;
               break;

            case 1:
               ch |= ((p >> 4) & 0x03);
               result.addElement(new Integer(ch));
               ch = (p << 4) & 0xF0;
               break;

            case 2:
               ch |= ((p >> 2) & 0x0F);
               result.addElement(new Integer(ch));
               ch = (p << 6) & 0xC0;
               break;

            case 3:
               ch |= (p & 0x3F);
               result.addElement(new Integer(ch));
               break;
         }
         state = (state + 1) % 4;
      }

      // Remove the trailing nulls

      int nPad = 0;
      for (int i = buffer.length()-1; i >= 0; i--) {
         char c = buffer.charAt(i);
         if (c == '=')
            nPad++;
         else break;
      }
      int n = result.size() - nPad;
      if (n < 0)
         n = 0;

      // Return the byte array

      byte[] bytes = new byte[n];
      for (int i = 0; i < n; i++)
         bytes[i] =
            ((Integer) result.elementAt(i)).byteValue();

      return bytes;
   }
   
   /**
    * Decodes the given base64 string to bytes.
    * This method version uses an "URL-safe" set of base64 characters, containing only alphanumeric character plus
    * characters "-" and "_".
    * Be sure that the base64 string was created with the same character set when using it (by encodeWeb for example).
    * @param buffer the string to be decoded
    * @return the decoded byte array
 * @throws IOException 
    * @exception java.io.IOException if the buffer
    * contains invalid data
    */
    public static byte[] decodeWeb(String buffer) throws IOException
    {
       if ((buffer.length() % 4) != 0)
          throw new IOException
          ("Buffer length not a multiple of four");

       int state = 0;
       int ch = 0;
       Vector<Integer> result = new Vector<Integer>();
       for (int i = 0; i < buffer.length(); i++) {
          int b = buffer.charAt(i);
          int p = base64TableWeb.indexOf(b);
          if (p == -1)
             if (b == '=')
                p = 0;
             else
                throw new IOException
                ("Invalid character in input");

          switch (state) {

             case 0:
                ch = (p << 2) & 0xFC;
                break;

             case 1:
                ch |= ((p >> 4) & 0x03);
                result.addElement(new Integer(ch));
                ch = (p << 4) & 0xF0;
                break;

             case 2:
                ch |= ((p >> 2) & 0x0F);
                result.addElement(new Integer(ch));
                ch = (p << 6) & 0xC0;
                break;

             case 3:
                ch |= (p & 0x3F);
                result.addElement(new Integer(ch));
                break;
          }
          state = (state + 1) % 4;
       }

       // Remove the trailing nulls

       int nPad = 0;
       for (int i = buffer.length()-1; i >= 0; i--) {
          char c = buffer.charAt(i);
          if (c == '=')
             nPad++;
          else break;
       }
       int n = result.size() - nPad;
       if (n < 0)
          n = 0;

       // Return the byte array

       byte[] bytes = new byte[n];
       for (int i = 0; i < n; i++)
          bytes[i] =
             ((Integer) result.elementAt(i)).byteValue();

       return bytes;
    }   
   
      /**
     * Encodes bytes to a base64 String.
     * This method version uses the "traditional" set of base64 characters that are not all URL-safe,
     * containing alphanumeric characters plus characters "/" and "+".
     * Don't use this method version if the base64 string is to be used in URLs or other ASCII-only places. 
     * Use encodeWeb instead.
     * @param buffer
     * @return The encoded byte buffer
     */
    public static String encode(byte[] buffer)
   {
      StringBuffer result = new StringBuffer();
      int state = 0;
      int n = buffer.length;
      int index = 0;

      for (int i = 0; i < n; i++) {
         int c = buffer[i] & 0xFF;

         // Take three bytes of input = 24 bits
         // Split it into 4 chunks of six bits each
         // Treat these chunks as indices into the
         // base64 table above.

         switch (state) {

            case 0:
               index = (c >> 2) & 0x3F;
               result.append(base64Table.charAt(index));
               index = (c << 4) & 0x30;
               break;

            case 1:
               index |= (c >> 4) & 0x0F;
               result.append(base64Table.charAt(index));
               index = (c << 2) & 0x3C;
               break;

            case 2:
               index |= (c >> 6) & 0x03;
               result.append(base64Table.charAt(index));
               index = c & 0x3F;
               result.append(base64Table.charAt(index));
               break;
         }
         state = (state + 1) % 3;
      }

      // Complete the string with zero bits
      // and pad with "=" characters as necessary

      switch (state) {
         case 0:
            // No padding necessary
            break;
         case 1:
            result.append(base64Table.charAt(index));
            result.append('=');
            result.append('=');
            break;
         case 2:
            result.append(base64Table.charAt(index));
            result.append('=');
            break;
      }

      return result.toString();
   }
      
      /**
       * encodes the given buffer and returns an URLEncoded Base64 String
       * This method version uses an "URL-safe" set of base64 characters.
       * So base64 strings produced by it are safe to be used in URLs and other ASCII-only places.
       * You should generally prefer this method for base64 creation if decoding is also in your control.
       * Use decodeWeb to decode base64 strings created by this method.       
       * @param buffer
       * @return URLEncoded Base64 String
     * @throws UnsupportedEncodingException 
       */
      public static String encodeWeb(byte[] buffer)
      {
         StringBuffer result = new StringBuffer();
         int state = 0;
         int n = buffer.length;
         int index = 0;

         for (int i = 0; i < n; i++) {
            int c = buffer[i] & 0xFF;

            // Take three bytes of input = 24 bits
            // Split it into 4 chunks of six bits each
            // Treat these chunks as indices into the
            // base64 table above.

            switch (state) {

               case 0:
                  index = (c >> 2) & 0x3F;
                  result.append(base64TableWeb.charAt(index));
                  index = (c << 4) & 0x30;
                  break;

               case 1:
                  index |= (c >> 4) & 0x0F;
                  result.append(base64TableWeb.charAt(index));
                  index = (c << 2) & 0x3C;
                  break;

               case 2:
                  index |= (c >> 6) & 0x03;
                  result.append(base64TableWeb.charAt(index));
                  index = c & 0x3F;
                  result.append(base64TableWeb.charAt(index));
                  break;
            }
            state = (state + 1) % 3;
         }

         // Complete the string with zero bits
         // and pad with "=" characters as necessary

         switch (state) {
            case 0:
               // No padding necessary
               break;
            case 1:
               result.append(base64TableWeb.charAt(index));
               result.append('=');
               result.append('=');
               break;
            case 2:
               result.append(base64TableWeb.charAt(index));
               result.append('=');
               break;
         }

         return result.toString();
      }

    public static final String ENCODING_KEY = "base64";

    public String decodePassword(String password) throws PasswordEncodingException {
        try {
            return new String(decodeWeb(password), "UTF-8");
        }
        catch (Exception e) {
            throw new PasswordEncodingException("Exception decoding password", e);
        }
    }

    public String encodePassword(String password) throws PasswordEncodingException {
        try {
            return encodeWeb(password.getBytes("UTF-8"));
        }
        catch (UnsupportedEncodingException e) {
            throw new PasswordEncodingException("Exception encoding password", e);
        }
    }

    public String getEncodingKey() {
        return ENCODING_KEY;
    }      
      
}
