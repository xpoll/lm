package cn.blmdz.home.search.api.query;

import cn.blmdz.home.search.api.query.Criteria;

public class Range extends Criteria {
   private static final long serialVersionUID = 8008419030673295131L;
   private Object low;
   private Object high;

   public Range(String field, Object low, Object high) {
      super(field);
      this.low = low;
      this.high = high;
   }

   public Object getLow() {
      return this.low;
   }

   public Object getHigh() {
      return this.high;
   }
}
