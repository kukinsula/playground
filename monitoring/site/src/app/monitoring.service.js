"use strict";
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
var __metadata = (this && this.__metadata) || function (k, v) {
    if (typeof Reflect === "object" && typeof Reflect.metadata === "function") return Reflect.metadata(k, v);
};
var core_1 = require("@angular/core");
var http_1 = require("@angular/http");
var Rx_1 = require("rxjs/Rx");
var MonitoringService = (function () {
    function MonitoringService(http) {
        this.http = http;
        if (window['source'] != undefined) {
            this.source = window['source'];
        }
    }
    MonitoringService.prototype.getOverview = function () {
        return this.getObservable('overview');
    };
    MonitoringService.prototype.getCPU = function () {
        return this.getObservable('cpu');
    };
    MonitoringService.prototype.getMemory = function () {
        return this.getObservable('mem');
    };
    MonitoringService.prototype.getNetwork = function () {
        return this.getObservable('net');
    };
    MonitoringService.prototype.removeEventListener = function (name) {
        this.source.removeEventListener(name);
    };
    MonitoringService.prototype.eventHandler = function (msg) {
    };
    MonitoringService.prototype.getObservable = function (name) {
        var _this = this;
        if (this.source == undefined) {
        }
        return Rx_1.Observable.create(function (sub) {
            _this.source.addEventListener(name, function (msg) {
                sub.next(JSON.parse(msg.data));
            });
        });
    };
    return MonitoringService;
}());
MonitoringService = __decorate([
    core_1.Injectable(),
    __metadata("design:paramtypes", [http_1.Http])
], MonitoringService);
exports.MonitoringService = MonitoringService;
//# sourceMappingURL=monitoring.service.js.map