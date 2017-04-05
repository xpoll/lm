package io.terminus.parana.common.utils;

import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import javax.annotation.Nullable;

public class Iters {
   public static Iterable nullToEmpty(@Nullable Iterable iter) {
      return iter == null?new Iterable() {
         public Iterator iterator() {
            return Collections.emptyIterator();
         }
      }:iter;
   }

   public static List nullToEmpty(@Nullable List list) {
      return list == null?Collections.emptyList():list;
   }

   public static Map nullToEmpty(@Nullable Map map) {
      return map == null?Collections.emptyMap():map;
   }

   public static Iterable emptyToNull(@Nullable Iterable iter) {
      return iter != null && iter.iterator().hasNext()?iter:null;
   }

   public static List emptyToNull(@Nullable List list) {
      return list != null && !list.isEmpty()?list:null;
   }

   public static Object[] emptyToNull(@Nullable Object[] array) {
      return array != null && array.length > 0?array:null;
   }

   public static long[] emptyToNull(@Nullable long[] array) {
      return array != null && array.length > 0?array:null;
   }
}
