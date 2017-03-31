package cn.blmdz.home.search.api.query;

import java.io.Serializable;

class Element implements Serializable {
   private final Object value;
   private boolean last;

   public Element(Object value) {
      this.value = value;
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
