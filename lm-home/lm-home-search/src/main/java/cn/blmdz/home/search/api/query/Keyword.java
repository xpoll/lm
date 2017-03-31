package cn.blmdz.home.search.api.query;

import com.google.common.collect.Lists;
import java.io.Serializable;
import java.util.List;
import org.springframework.util.CollectionUtils;

public class Keyword implements Serializable {
   private static final long serialVersionUID = 18857107519656339L;
   private boolean multiField;
   private final List fields;

   public Keyword(List<String> fieldList, Object value) {
      if(CollectionUtils.isEmpty(fieldList)) {
         throw new IllegalArgumentException("no field specified");
      } else {
         this.fields = Lists.newArrayListWithCapacity(fieldList.size());

         for(String field : fieldList) {
            Keyword.Field f = new Keyword.Field(field, value);
            this.fields.add(f);
         }

         if(this.fields.size() > 1) {
            this.multiField = true;
         }

      }
   }

   public boolean isMultiField() {
      return this.multiField;
   }

   public List getFields() {
      return this.fields;
   }

   static class Field implements Serializable {
      private static final long serialVersionUID = -8500243762169822933L;
      private final String field;
      private final Object value;
      private boolean last;

      public Field(String field, Object value) {
         this.field = field;
         this.value = value;
      }

      public String getField() {
         return this.field;
      }

      public Object getValue() {
         return this.value;
      }

      public boolean isLast() {
         return this.last;
      }

      public void setLast(boolean last) {
         this.last = last;
      }
   }
}
