package cn.blmdz.wolf.auth.util;

import java.util.List;

import com.google.common.base.Strings;
import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Lists;
import com.google.common.collect.Multimap;

import cn.blmdz.wolf.auth.model.ExtendedRole;

public class ExtendedRoleUtil {
   public static Multimap groupUpRoles(List<String> roles) {
      Multimap<String, ExtendedRole> map = ArrayListMultimap.create();

      for(String role : roles) {
         List<String> parsedRole = roleConsFrom(role);
         if(parsedRole.size() > 1) {
            String baseRole = (String)parsedRole.get(0);
            ExtendedRole er = new ExtendedRole();
            er.setBase(baseRole.toUpperCase());
            boolean foundLevel = false;
            String level = null;
            List<Long> levelContext = Lists.newArrayList();
            Multimap<String, String> otherContext = ArrayListMultimap.create();

            for(int i = 1; i < parsedRole.size(); ++i) {
               List<String> inner = roleConsFrom((String)parsedRole.get(i));
               String context = (String)inner.get(0);
               if("OWNER".equalsIgnoreCase(context)) {
                  level = "OWNER";
                  foundLevel = true;
               } else if("SUB".equalsIgnoreCase(context)) {
                  level = "SUB";
                  foundLevel = true;

                  for(int j = 1; j < inner.size(); ++j) {
                     Long roleId = Long.valueOf(Long.parseLong((String)inner.get(j)));
                     levelContext.add(roleId);
                  }
               } else if(inner.size() > 0) {
                  for(int j = 1; j < inner.size(); ++j) {
                     otherContext.put(context, inner.get(j));
                  }
               }
            }

            if(foundLevel) {
               er.setLevel(level);
               er.setLevelContext(levelContext);
               er.setOtherContext(otherContext);
               map.put(baseRole, er);
            }
         }
      }

      return map;
   }

   public static List roleConsFrom(String str) {
      if(Strings.isNullOrEmpty(str)) {
         throw new RuntimeException("invalid role");
      } else {
         List<String> result = Lists.newArrayList();

         for(int i = 0; i < str.length(); ++i) {
            if(str.charAt(i) == 40) {
               String inner = str.substring(i + 1, str.length() - 1);
               result.add(str.substring(0, i));
               result.addAll(roleListHelper(inner));
               return result;
            }
         }

         result.add(str);
         return result;
      }
   }

   private static List roleListHelper(String list) {
      if(Strings.isNullOrEmpty(list)) {
         return Lists.newArrayList();
      } else {
         for(int i = 0; i < list.length(); ++i) {
            if(list.charAt(i) == 44) {
               List<String> result = Lists.newArrayList();
               result.add(list.substring(0, i));
               result.addAll(roleListHelper(list.substring(i + 1)));
               return result;
            }
         }

         return Lists.newArrayList(new String[]{list});
      }
   }
}
