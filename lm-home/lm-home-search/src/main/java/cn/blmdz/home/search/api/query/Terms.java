package cn.blmdz.home.search.api.query;

import com.google.common.collect.Lists;

import cn.blmdz.home.search.api.query.Criteria;
import cn.blmdz.home.search.api.query.Element;

import java.util.List;

public class Terms extends Criteria {
   private static final long serialVersionUID = -5060974023144224546L;
   private List values;

   public Terms(String field, List values) {
      super(field);
      List<Element> elements = Lists.newArrayListWithCapacity(values.size());

      for(Object value : values) {
         Element element = new Element(value);
         elements.add(element);
      }

      this.values = elements;
   }

   public List getValues() {
      return this.values;
   }
}
