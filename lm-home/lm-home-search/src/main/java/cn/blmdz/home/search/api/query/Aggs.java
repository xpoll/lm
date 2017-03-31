package cn.blmdz.home.search.api.query;

import java.io.Serializable;

public class Aggs implements Serializable {
   private static final long serialVersionUID = 463975846365165897L;
   private final String id;
   private final String field;
   private final Integer size;
   private boolean last;

   public Aggs(String id, String field, Integer size) {
      this.id = id;
      this.field = field;
      this.size = size;
   }

   public String getId() {
      return this.id;
   }

   public String getField() {
      return this.field;
   }

   public Integer getSize() {
      return this.size;
   }

   public boolean isLast() {
      return this.last;
   }

   public void setLast(boolean last) {
      this.last = last;
   }
}
