package io.terminus.galaxy.web.front.controller;

import io.terminus.parana.user.address.model.Address;
import io.terminus.parana.user.address.service.AddressReadService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * 地址信息
 *
 * Created by IntelliJ IDEA.
 * Author: luoys
 * Date: 11:16 16/6/30
 */
@Slf4j
@RestController
@RequestMapping("/api/address")
public class Addresses {

    private AddressReadService addressReadService;

    @Autowired
    public Addresses(AddressReadService addressReadService){
        this.addressReadService = addressReadService;
    }

    @RequestMapping(value = "/{addressId}/children", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
    public List<Address> childAddressOf(@PathVariable Integer addressId){
        return addressReadService.childAddressOf(addressId).getResult();
    }
}
