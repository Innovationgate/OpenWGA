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

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

/**
 * 
 * This abstract class implements the recommended behaviour to resolve hierarchical group membership
 * (groups that themselves maybe members of groups) without determining the functionality for resolving
 * direct group memberships.
 * <p>
 * The behaviour is:
 * <ol>
 * <li>Determine the direct group membership for the member to resolve
 * <li>For each group found determine the direct membership again
 * <li>Remove groups from the result lists that were already resolved
 * <li>If the result list is > 0 start over at point 2 with the new result list
 * <li>If the result list is = 0, resolving is over and return the retrieved groups.
 * </ol>
 * </p>
 * <p>
 * This behaviour is 
 * <ul>
 * <li>a) a very effective way for hierarchical resolving that 
 * <li>b) is not vulnerable to circular group memberships (a is member of b is member of a and the like)   
 * </ul>
 * </p>
 * <p>
 * To use this just
 * <ul>
 * <li>a) Subclass it, set MembershipEntity to be any entity representing a membership entity, f.e. String for simple user/group name
 * <li>b) Write {@link #resolveDirectMembership(String)} to determine direct group membership of the given member
 * <li>c) In your code call {@link #resolveMembership(String)} to determine hierarchical group membership
 * </ul>
 * </p>
 */
public abstract class AnyTypeGroupMembershipResolver<MembershipEntity> {
    
    /**
     * Determine hierarchical group membership of the given member
     * @param member The member name whose groups should be determined
     * @return List of names of those groups that the member name is member of 
     * @throws GroupResolvingException
     */
    public Set<MembershipEntity> resolveMembership(MembershipEntity member) throws GroupResolvingException {
        
        Set<MembershipEntity> groups = new HashSet<MembershipEntity>();
        Set<MembershipEntity> membersToEvaluate = new HashSet<MembershipEntity>();
        membersToEvaluate.add(member);
        
        while (true) {
            
            Set<MembershipEntity> newMembers = new HashSet<MembershipEntity>();
            Iterator<MembershipEntity> membersIt = membersToEvaluate.iterator();
            while (membersIt.hasNext()) {
                newMembers.addAll(resolveDirectMembership(membersIt.next()));
            }
            
            newMembers.removeAll(groups);
            
            if (newMembers.size() > 0) {
                for (MembershipEntity newMember: newMembers) {
                    groups.add(newMember);  
                }
                membersToEvaluate = newMembers;
            }
            else {
                break;
            }
            
        }
        
        
        return groups;
        
    }
    
    /**
     * Abstract method to define how direct group membership is determined. Direct group membership is
     * defined as a person/group A being a directly defined member of a group B (in contrary to indirect membership
     * where a person/group A is a member of a group B, because another group C that contains A is a member of B) 
     * <p> 
     * Implement your indivual method of determining direct group membership for the given member here.
     * In traditional grouping systems where a group consists only of a list of user/group names as members
     * this would be: Look if the member name is contained in the group members list.
     * If something essential goes wrong in your code throw a {@link GroupResolvingException}.
     * </p>
     * @param member The member name to determine direct group membership for
     * @return A set of names of those groups that the member name is member of
     * @throws GroupResolvingException
     */
    public abstract Set<MembershipEntity> resolveDirectMembership(MembershipEntity member) throws GroupResolvingException;

}
