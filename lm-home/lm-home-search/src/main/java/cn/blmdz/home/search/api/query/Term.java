package cn.blmdz.home.search.api.query;

import cn.blmdz.home.search.api.query.Criteria;

public class Term extends Criteria {
   private static final long serialVersionUID = -2574958724048095177L;
   private Object value;

   public Term(String field, Object value) {
      super(field);
      this.value = value;
   }

   public Object getValue() {
      return this.value;
   }
}
