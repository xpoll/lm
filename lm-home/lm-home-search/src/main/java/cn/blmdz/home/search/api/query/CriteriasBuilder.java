package cn.blmdz.home.search.api.query;

import java.util.List;

import org.springframework.util.CollectionUtils;

import cn.blmdz.home.search.api.query.Aggs;
import cn.blmdz.home.search.api.query.Criterias;
import cn.blmdz.home.search.api.query.Element;
import cn.blmdz.home.search.api.query.Highlight;
import cn.blmdz.home.search.api.query.Keyword;
import cn.blmdz.home.search.api.query.Range;
import cn.blmdz.home.search.api.query.Sort;
import cn.blmdz.home.search.api.query.Term;
import cn.blmdz.home.search.api.query.Terms;

public class CriteriasBuilder {
   private int from = 0;
   private int size = 20;
   private boolean hasFilter;
   private boolean hasSort;
   private boolean hasAggs;
   private boolean hasHighlight;
   private Keyword keyword;
   private List term;
   private List<Terms> terms;
   private List ranges;
   private List sorts;
   private List aggs;
   private List highlights;

   public CriteriasBuilder withPageInfo(int from, int size) {
      this.from = from;
      this.size = size;
      return this;
   }

   public CriteriasBuilder withTerm(List termList) {
      if(!CollectionUtils.isEmpty(termList)) {
         this.term = termList;
         this.hasFilter = true;
      }

      return this;
   }

   public CriteriasBuilder withTerms(List termsList) {
      if(!CollectionUtils.isEmpty(termsList)) {
         this.terms = termsList;

         for(Terms tms : this.terms) {
            List<Element> values = tms.getValues();
            if(values != null && !values.isEmpty()) {
               Element element = (Element)values.get(values.size() - 1);
               element.setLast(true);
            }
         }

         this.hasFilter = true;
      }

      return this;
   }

   public CriteriasBuilder withRanges(List rangeList) {
      if(!CollectionUtils.isEmpty(rangeList)) {
         this.ranges = rangeList;
         this.hasFilter = true;
      }

      return this;
   }

   public CriteriasBuilder withSorts(List sortList) {
      if(!CollectionUtils.isEmpty(sortList)) {
         this.sorts = sortList;
         this.hasSort = true;
         Sort s = (Sort)sortList.get(sortList.size() - 1);
         s.setLast(true);
      }

      return this;
   }

   public CriteriasBuilder withAggs(List aggsList) {
      if(!CollectionUtils.isEmpty(aggsList)) {
         this.aggs = aggsList;
         this.hasAggs = true;
         Aggs a = (Aggs)this.aggs.get(this.aggs.size() - 1);
         a.setLast(true);
      }

      return this;
   }

   public CriteriasBuilder withHighlights(List highlightList) {
      if(!CollectionUtils.isEmpty(highlightList)) {
         this.highlights = highlightList;
         this.hasHighlight = true;
         Highlight h = (Highlight)this.highlights.get(this.highlights.size() - 1);
         h.setLast(true);
      }

      return this;
   }

   public CriteriasBuilder withKeyword(Keyword keyword) {
      if(keyword != null) {
         this.keyword = keyword;
         List<Keyword.Field> fields = keyword.getFields();
         if(!CollectionUtils.isEmpty(fields)) {
            Keyword.Field field = (Keyword.Field)fields.get(fields.size() - 1);
            field.setLast(true);
         }
      }

      return this;
   }

   public Criterias build() {
      if(this.ranges != null && !this.ranges.isEmpty()) {
         Range r = (Range)this.ranges.get(this.ranges.size() - 1);
         r.setLast(true);
      } else if(this.terms != null && !this.terms.isEmpty()) {
         Terms t = (Terms)this.terms.get(this.terms.size() - 1);
         t.setLast(true);
      } else if(this.term != null && !this.term.isEmpty()) {
         Term t = (Term)this.term.get(this.term.size() - 1);
         t.setLast(true);
      }

      return new Criterias(this);
   }

   public int getFrom() {
      return this.from;
   }

   public int getSize() {
      return this.size;
   }

   public boolean isHasFilter() {
      return this.hasFilter;
   }

   public boolean isHasSort() {
      return this.hasSort;
   }

   public boolean isHasAggs() {
      return this.hasAggs;
   }

   public boolean isHasHighlight() {
      return this.hasHighlight;
   }

   public Keyword getKeyword() {
      return this.keyword;
   }

   public List getTerm() {
      return this.term;
   }

   public List getTerms() {
      return this.terms;
   }

   public List getRanges() {
      return this.ranges;
   }

   public List getSorts() {
      return this.sorts;
   }

   public List getAggs() {
      return this.aggs;
   }

   public List getHighlights() {
      return this.highlights;
   }
}
