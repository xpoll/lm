package cn.blmdz.wolf.search.impl.shop.impl;

import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

import com.google.common.base.Throwables;

import cn.blmdz.home.common.model.Paging;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.home.search.api.Searcher;
import cn.blmdz.home.search.api.model.WithAggregations;
import cn.blmdz.home.search.api.query.Criterias;
import cn.blmdz.wolf.parana.search.dto.SearchedShop;
import cn.blmdz.wolf.parana.search.dto.SearchedShopWithAggs;
import cn.blmdz.wolf.parana.search.shop.SearchShopProperties;
import cn.blmdz.wolf.parana.search.shop.ShopSearchReadService;

@Service
public class ShopSearchReadServiceImpl
  implements ShopSearchReadService
{
  private static final Logger log = LoggerFactory.getLogger(ShopSearchReadServiceImpl.class);
  private final SearchShopProperties searchShopProperties;
  private final Searcher searcher;
  private final BaseShopQueryBuilder shopQueryBuilder;

  @Autowired
  public ShopSearchReadServiceImpl(SearchShopProperties searchShopProperties, Searcher searcher, BaseShopQueryBuilder shopQueryBuilder)
  {
    this.searchShopProperties = searchShopProperties;
    this.searcher = searcher;
    this.shopQueryBuilder = shopQueryBuilder;
  }

  public Response<SearchedShopWithAggs> searchWithAggs(Integer pageNo, Integer pageSize, String templateName, Map<String, String> params)
  {
    try
    {
      String q = (String)params.get("q");
      if (StringUtils.hasText(q)) {
        params.put("highlight", "name");
      }

      Criterias criterias = this.shopQueryBuilder.makeCriterias(pageNo, pageSize, params);
      WithAggregations withAggs = this.searcher.searchWithAggs(this.searchShopProperties.getIndexName(), this.searchShopProperties.getIndexType(), templateName, criterias, SearchedShop.class);

      SearchedShopWithAggs searchWithAggs = new SearchedShopWithAggs();
      Paging entities = new Paging(withAggs.getTotal(), withAggs.getData());
      searchWithAggs.setEntities(entities);

      return Response.ok(searchWithAggs);
    } catch (Exception e) {
      log.error("fail to search shops,pageNo={},pageSize={},templateName={},params={},cause:{}", new Object[] { pageNo, pageSize, templateName, params, Throwables.getStackTraceAsString(e) });
    }
    return Response.fail("shop.search.fail");
  }
}