package io.terminus.galaxy.web.shop;

import com.google.common.collect.ImmutableMap;
import io.terminus.galaxy.web.component.GalaxyMainSearch;
import io.terminus.parana.search.dto.SearchedShopWithAggs;
import io.terminus.parana.search.shop.ShopDumpService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/**
 * @Author xgs.
 * @Email xgs@terminus.io
 * @Date 16/8/2
 */

@Slf4j
@RestController
@RequestMapping("/api/shop/search")
public class ShopSearches {

    private final ShopDumpService shopDumpService;

    private final GalaxyMainSearch galaxyMainSearch;

    @Autowired
    public ShopSearches(ShopDumpService shopDumpService, GalaxyMainSearch galaxyMainSearch) {
        this.shopDumpService = shopDumpService;
        this.galaxyMainSearch = galaxyMainSearch;
    }

    @RequestMapping("dump")
    public Boolean dump(){
        return shopDumpService.fullDump().getResult();
    }

    @RequestMapping(method = RequestMethod.GET)
    public SearchedShopWithAggs search(@RequestParam("q")String q){
        return galaxyMainSearch.searchedShopWithAggs(null,null, ImmutableMap.<String, String>of("q", q)).getResult();
    }

    @RequestMapping(value = "test", method = RequestMethod.GET)
    public SearchedShopWithAggs test(){
        return galaxyMainSearch.searchedShopWithAggs(null,null, ImmutableMap.<String, String>of("q", "天猫")).getResult();
    }
}
